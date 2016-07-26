{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Text.Ginger.Run.FuncUtils
where

import Prelude ( (.), ($), (==), (/=)
               , (>), (<), (>=), (<=)
               , (+), (-), (*), (/), div, (**), (^)
               , (||), (&&)
               , (++)
               , Show, show
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , Int, Integer, String
               , fromIntegral, floor, round
               , not
               , show
               , uncurry
               , seq
               , fst, snd
               , maybe
               , Either (..)
               , id
               )
import qualified Prelude
import Data.Maybe (fromMaybe, isJust)
import qualified Data.List as List
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.GVal
import Text.Ginger.Run.Type
import Text.Printf
import Text.PrintfA
import Data.Scientific (formatScientific)

import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Scientific as Scientific
import Data.Default (def)
import Safe (readMay, lastDef, headMay)
import Network.HTTP.Types (urlEncode)
import Debug.Trace (trace)
import Data.Maybe (isNothing)
import Data.List (lookup, zipWith, unzip)

unaryFunc :: forall m h. (Monad m) => (GVal (Run m h) -> GVal (Run m h)) -> Function (Run m h)
unaryFunc f [] = return def
unaryFunc f ((_, x):[]) = return (f x)

ignoreArgNames :: ([a] -> b) -> ([(c, a)] -> b)
ignoreArgNames f args = f (Prelude.map snd args)

variadicNumericFunc :: Monad m => Scientific -> ([Scientific] -> Scientific) -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
variadicNumericFunc zero f args =
    return . toGVal . f $ args'
    where
        args' :: [Scientific]
        args' = Prelude.map (fromMaybe zero . asNumber . snd) args

unaryNumericFunc :: Monad m => Scientific -> (Scientific -> Scientific) -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
unaryNumericFunc zero f args =
    return . toGVal . f $ args'
    where
        args' :: Scientific
        args' = case args of
                    [] -> 0
                    (arg:_) -> fromMaybe zero . asNumber . snd $ arg

variadicStringFunc :: Monad m => ([Text] -> Text) -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
variadicStringFunc f args =
    return . toGVal . f $ args'
    where
        args' :: [Text]
        args' = Prelude.map (asText . snd) args

-- | Match args according to a given arg spec, Python style.
-- The return value is a triple of @(matched, args, kwargs, unmatchedNames)@,
-- where @matches@ is a hash map of named captured arguments, args is a list of
-- remaining unmatched positional arguments, kwargs is a list of remaining
-- unmatched named arguments, and @unmatchedNames@ contains the argument names
-- that haven't been matched.
extractArgs :: [Text] -> [(Maybe Text, a)] -> (HashMap Text a, [a], HashMap Text a, [Text])
extractArgs argNames args =
    let (matchedPositional, argNames', args') = matchPositionalArgs argNames args
        (matchedKeyword, argNames'', args'') = matchKeywordArgs argNames' args'
        unmatchedPositional = [ a | (Nothing, a) <- args'' ]
        unmatchedKeyword = HashMap.fromList [ (k, v) | (Just k, v) <- args'' ]
    in ( HashMap.fromList (matchedPositional ++ matchedKeyword)
       , unmatchedPositional
       , unmatchedKeyword
       , argNames''
       )
    where
        matchPositionalArgs :: [Text] -> [(Maybe Text, a)] -> ([(Text, a)], [Text], [(Maybe Text, a)])
        matchPositionalArgs [] args = ([], [], args)
        matchPositionalArgs names [] = ([], names, [])
        matchPositionalArgs names@(n:ns) allArgs@((anm, arg):args)
            | Just n == anm || isNothing anm =
                let (matched, ns', args') = matchPositionalArgs ns args
                in ((n, arg):matched, ns', args')
            | otherwise = ([], names, allArgs)

        matchKeywordArgs :: [Text] -> [(Maybe Text, a)] -> ([(Text, a)], [Text], [(Maybe Text, a)])
        matchKeywordArgs [] args = ([], [], args)
        matchKeywordArgs names allArgs@((Nothing, arg):args) =
            let (matched, ns', args') = matchKeywordArgs names args
            in (matched, ns', (Nothing, arg):args')
        matchKeywordArgs names@(n:ns) args =
            case (lookup (Just n) args) of
                Nothing ->
                    let (matched, ns', args') = matchKeywordArgs ns args
                    in (matched, n:ns', args')
                Just v ->
                    let args' = [ (k,v) | (k,v) <- args, k /= Just n ]
                        (matched, ns', args'') = matchKeywordArgs ns args'
                    in ((n,v):matched, ns', args'')

-- | Parse argument list into type-safe argument structure.
extractArgsT :: ([Maybe a] -> b) -> [Text] -> [(Maybe Text, a)] -> Either ([a], HashMap Text a, [Text]) b
extractArgsT f argNames args =
    let (matchedMap, freeArgs, freeKwargs, unmatched) = extractArgs argNames args
    in if List.null freeArgs && HashMap.null freeKwargs
        then Right (f $ fmap (\name -> HashMap.lookup name matchedMap) argNames)
        else Left (freeArgs, freeKwargs, unmatched)

-- | Parse argument list into flat list of matched arguments.
extractArgsL :: [Text] -> [(Maybe Text, a)] -> Either ([a], HashMap Text a, [Text]) [Maybe a]
extractArgsL = extractArgsT id

extractArgsDefL :: [(Text, a)] -> [(Maybe Text, a)] -> Either ([a], HashMap Text a, [Text]) [a]
extractArgsDefL argSpec args =
    let (names, defs) = unzip argSpec
    in injectDefaults defs <$> extractArgsL names args

injectDefaults :: [a] -> [Maybe a] -> [a]
injectDefaults = zipWith fromMaybe
