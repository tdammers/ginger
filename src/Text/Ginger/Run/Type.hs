{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Text.Ginger.Run.Type
( GingerContext (..)
, makeContext
, makeContextM
, makeContext'
, makeContextM'
, makeContextHtml
, makeContextHtmlM
, makeContextText
, makeContextTextM
, easyContext
, ContextEncodable (..)
, liftRun
, liftRun2
, Run (..)
, RunState (..)
-- * The Newlines type
-- | Required for handling indentation
, Newlines (..)
)
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
import Text.Printf
import Text.PrintfA
import Data.Scientific (formatScientific)

import Data.Char (isSpace)
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

-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m h
    = GingerContext
        { contextLookup :: VarName -> Run m h (GVal (Run m h))
        , contextWrite :: h -> Run m h ()
        , contextEncode :: GVal (Run m h) -> h
        , contextNewlines :: Maybe (Newlines h)
        }

contextWriteEncoded :: GingerContext m h -> GVal (Run m h) -> Run m h ()
contextWriteEncoded context =
    contextWrite context . contextEncode context

easyContext :: (Monad m, ContextEncodable h, ToGVal (Run m h) v)
            => (h -> m ())
            -> v
            -> GingerContext m h
easyContext emit context =
    makeContextM'
        (\varName ->
            return
                (lookupLooseDef def
                    (toGVal varName)
                    (toGVal context)))
        emit
        encode
        newlines


-- | Typeclass that defines how to encode 'GVal's into a given type.
class ContextEncodable h where
    encode :: forall m. GVal m -> h
    newlines :: Maybe (Newlines h)
    newlines = Nothing

-- | Encoding to text just takes the text representation without further
-- processing.
instance ContextEncodable Text where
    encode = asText
    newlines = Just textNewlines

-- | Encoding to Html is implemented as returning the 'asHtml' representation.
instance ContextEncodable Html where
    encode = toHtml
    newlines = Just htmlNewlines

-- | Create an execution context for runGingerT.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM' :: (Monad m, Functor m)
             => (VarName -> Run m h (GVal (Run m h)))
             -> (h -> m ())
             -> (GVal (Run m h) -> h)
             -> Maybe (Newlines h)
             -> GingerContext m h
makeContextM' lookupFn writeFn encodeFn newlines =
    GingerContext
        { contextLookup = lookupFn
        , contextWrite = liftRun2 writeFn
        , contextEncode = encodeFn
        , contextNewlines = newlines
        }

liftLookup :: (Monad m, ToGVal (Run m h) v) => (VarName -> m v) -> VarName -> Run m h (GVal (Run m h))
liftLookup f k = do
    v <- liftRun $ f k
    return . toGVal $ v

-- | Create an execution context for runGinger.
-- The argument is a lookup function that maps top-level context keys to ginger
-- values. 'makeContext' is a specialized version of 'makeContextM', targeting
-- the 'Writer' 'Html' monad (which is what is used for the non-monadic
-- template interpreter 'runGinger').
--
-- The type of the lookup function may look intimidating, but in most cases,
-- marshalling values from Haskell to Ginger is a matter of calling 'toGVal'
-- on them, so the 'GVal (Run (Writer Html))' part can usually be ignored.
-- See the 'Text.Ginger.GVal' module for details.
makeContext' :: Monoid h
            => (VarName -> GVal (Run (Writer h) h))
            -> (GVal (Run (Writer h) h) -> h)
            -> Maybe (Newlines h)
            -> GingerContext (Writer h) h
makeContext' lookupFn =
    makeContextM'
        (return . lookupFn)
        tell

{-#DEPRECATED makeContext "Compatibility alias for makeContextHtml" #-}
makeContext :: (VarName -> GVal (Run (Writer Html) Html))
            -> GingerContext (Writer Html) Html
makeContext = makeContextHtml

{-#DEPRECATED makeContextM "Compatibility alias for makeContextHtmlM" #-}
makeContextM :: (Monad m, Functor m)
             => (VarName -> Run m Html (GVal (Run m Html)))
             -> (Html -> m ())
             -> GingerContext m Html
makeContextM = makeContextHtmlM

makeContextHtml :: (VarName -> GVal (Run (Writer Html) Html))
                -> GingerContext (Writer Html) Html
makeContextHtml l = makeContext' l toHtml (Just htmlNewlines)

makeContextHtmlM :: (Monad m, Functor m)
                 => (VarName -> Run m Html (GVal (Run m Html)))
                 -> (Html -> m ())
                 -> GingerContext m Html
makeContextHtmlM l w = makeContextM' l w toHtml (Just htmlNewlines)

makeContextText :: (VarName -> GVal (Run (Writer Text) Text))
                -> GingerContext (Writer Text) Text
makeContextText l = makeContext' l asText (Just textNewlines)

makeContextTextM :: (Monad m, Functor m)
                 => (VarName -> Run m Text (GVal (Run m Text)))
                 -> (Text -> m ())
                 -> GingerContext m Text
makeContextTextM l w = makeContextM' l w asText (Just textNewlines)

-- | A 'Newlines' determines the rules by which a 'h' value can be
-- split into lines, how a list of lines can be joined into a single
-- value, and how to remove leading whitespace.
data Newlines h =
    Newlines
        { splitLines :: h -> [h]
        , joinLines :: [h] -> h
        , stripIndent :: h -> h
        }

stringNewlines :: Newlines String
stringNewlines =
    Newlines
        { splitLines = List.lines
        , joinLines = List.unlines
        , stripIndent = List.dropWhile isSpace
        }

textNewlines :: Newlines Text
textNewlines =
    Newlines
        { splitLines = Text.lines
        , joinLines = Text.unlines
        , stripIndent = Text.stripStart
        }

htmlNewlines :: Newlines Html
htmlNewlines =
    Newlines
        { splitLines = fmap unsafeRawHtml . splitLines textNewlines . htmlSource
        , joinLines = unsafeRawHtml . joinLines textNewlines . fmap htmlSource
        , stripIndent = unsafeRawHtml . stripIndent textNewlines . htmlSource
        }


data RunState m h
    = RunState
        { rsScope :: HashMap VarName (GVal (Run m h))
        , rsCapture :: h
        , rsCurrentTemplate :: Template -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
        , rsIndentation :: Maybe [h] -- current indentation level, if any
        }

-- | Internal type alias for our template-runner monad stack.
type Run m h = StateT (RunState m h) (ReaderT (GingerContext m h) m)

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run m h a
liftRun = lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run m h b
liftRun2 f x = liftRun $ f x
