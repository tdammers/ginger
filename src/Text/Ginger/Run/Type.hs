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
, makeContextExM'
, makeContextHtml
, makeContextHtmlM
, makeContextHtmlExM
, makeContextText
, makeContextTextM
, makeContextTextExM
, easyContext
, ContextEncodable (..)
, liftRun
, liftRun2
, Run (..)
, RunState (..)
-- * The Newlines type
-- | Required for handling indentation
, Newlines (..)
-- * Hoisting
, hoistContext
, hoistRun
, hoistNewlines
, hoistRunState
, warn
, warnFromMaybe
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
data GingerContext p m h
    = GingerContext
        { contextLookup :: VarName -> Run p m h (GVal (Run p m h))
        , contextWrite :: h -> Run p m h ()
        , contextWarn :: Text -> Run p m h ()
        , contextEncode :: GVal (Run p m h) -> h
        , contextNewlines :: Maybe (Newlines h)
        }

-- | Hoist a context onto a different output type.
-- @hoistContext fwd rev context@ returns a context over a different
-- output type, applying the @fwd@ and @rev@ projections to convert
-- between the original and desired output types.
hoistContext :: Monad m => (h -> t) -> (t -> h) -> GingerContext p m h -> GingerContext p m t
hoistContext fwd rev c =
    GingerContext
        { contextLookup = \varName ->
            marshalGValEx
                (hoistRun fwd rev)
                (hoistRun rev fwd) <$>
                hoistRun fwd rev (contextLookup c varName)
        , contextWrite = \val ->
            hoistRun fwd rev (contextWrite c $ rev val)
        , contextWarn = \str ->
            hoistRun fwd rev (contextWarn c str)
        , contextEncode = \gval ->
            fwd .
                contextEncode c .
                marshalGValEx (hoistRun rev fwd) (hoistRun fwd rev) $
                gval
        , contextNewlines =
            hoistNewlines fwd rev <$> contextNewlines c
        }

contextWriteEncoded :: GingerContext p m h -> GVal (Run p m h) -> Run p m h ()
contextWriteEncoded context =
    contextWrite context . contextEncode context

easyContext :: (Monad m, ContextEncodable h, ToGVal (Run p m h) v)
            => (h -> m ())
            -> v
            -> GingerContext p m h
easyContext emit context =
    easyContextEx emit (Prelude.const $ return ()) context

easyContextEx :: (Monad m, ContextEncodable h, ToGVal (Run p m h) v)
              => (h -> m ())
              -> (Text -> m ())
              -> v
              -> GingerContext p m h
easyContextEx emit warn context =
    makeContextExM'
        (\varName ->
            return
                (lookupLooseDef def
                    (toGVal varName)
                    (toGVal context)))
        emit
        warn
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
             => (VarName -> Run p m h (GVal (Run p m h)))
             -> (h -> m ())
             -> (GVal (Run p m h) -> h)
             -> Maybe (Newlines h)
             -> GingerContext p m h
makeContextM' lookupFn writeFn encodeFn newlines =
  makeContextExM' lookupFn writeFn (Prelude.const $ return ()) encodeFn newlines

makeContextExM' :: (Monad m, Functor m)
             => (VarName -> Run p m h (GVal (Run p m h)))
             -> (h -> m ())
             -> (Text -> m ())
             -> (GVal (Run p m h) -> h)
             -> Maybe (Newlines h)
             -> GingerContext p m h
makeContextExM' lookupFn writeFn warnFn encodeFn newlines =
    GingerContext
        { contextLookup = lookupFn
        , contextWrite = liftRun2 writeFn
        , contextWarn = liftRun2 warnFn
        , contextEncode = encodeFn
        , contextNewlines = newlines
        }

liftLookup :: (Monad m, ToGVal (Run p m h) v) => (VarName -> m v) -> VarName -> Run p m h (GVal (Run p m h))
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
            => (VarName -> GVal (Run p (Writer h) h))
            -> (GVal (Run p (Writer h) h) -> h)
            -> Maybe (Newlines h)
            -> GingerContext p (Writer h) h
makeContext' lookupFn =
    makeContextM'
        (return . lookupFn)
        tell

{-#DEPRECATED makeContext "Compatibility alias for makeContextHtml" #-}
makeContext :: (VarName -> GVal (Run p (Writer Html) Html))
            -> GingerContext p (Writer Html) Html
makeContext = makeContextHtml

{-#DEPRECATED makeContextM "Compatibility alias for makeContextHtmlM" #-}
makeContextM :: (Monad m, Functor m)
             => (VarName -> Run p m Html (GVal (Run p m Html)))
             -> (Html -> m ())
             -> GingerContext p m Html
makeContextM = makeContextHtmlM

makeContextHtml :: (VarName -> GVal (Run p (Writer Html) Html))
                -> GingerContext p (Writer Html) Html
makeContextHtml l = makeContext' l toHtml (Just htmlNewlines)

makeContextHtmlM :: (Monad m, Functor m)
                 => (VarName -> Run p m Html (GVal (Run p m Html)))
                 -> (Html -> m ())
                 -> GingerContext p m Html
makeContextHtmlM l w = makeContextM' l w toHtml (Just htmlNewlines)

makeContextHtmlExM :: (Monad m, Functor m)
                 => (VarName -> Run p m Html (GVal (Run p m Html)))
                 -> (Html -> m ())
                 -> (Text -> m ())
                 -> GingerContext p m Html
makeContextHtmlExM l w warn = makeContextExM' l w warn toHtml (Just htmlNewlines)

makeContextText :: (VarName -> GVal (Run p (Writer Text) Text))
                -> GingerContext p (Writer Text) Text
makeContextText l = makeContext' l asText (Just textNewlines)

makeContextTextM :: (Monad m, Functor m)
                 => (VarName -> Run p m Text (GVal (Run p m Text)))
                 -> (Text -> m ())
                 -> GingerContext p m Text
makeContextTextM l w = makeContextM' l w asText (Just textNewlines)

makeContextTextExM :: (Monad m, Functor m)
                 => (VarName -> Run p m Text (GVal (Run p m Text)))
                 -> (Text -> m ())
                 -> (Text -> m ())
                 -> GingerContext p m Text
makeContextTextExM l w warn = makeContextExM' l w warn asText (Just textNewlines)

-- | A 'Newlines' determines the rules by which a 'h' value can be
-- split into lines, how a list of lines can be joined into a single
-- value, and how to remove leading whitespace.
data Newlines h =
    Newlines
        { splitLines :: h -> [h]
        , joinLines :: [h] -> h
        , stripIndent :: h -> h
        , endsWithNewline :: h -> Bool
        }

-- | Hoist a 'Newlines' onto a different output type.
-- You don't normally need to use this directly; see 'hoistRun' and/or
-- 'hoistContext'.
hoistNewlines :: (h -> t) -> (t -> h) -> Newlines h -> Newlines t
hoistNewlines fwd rev n =
    Newlines
        { splitLines = List.map fwd . splitLines n . rev
        , joinLines = fwd . joinLines n . List.map rev
        , stripIndent = fwd . stripIndent n . rev
        , endsWithNewline = endsWithNewline n . rev
        }

textNewlines :: Newlines Text
textNewlines =
    Newlines
        { splitLines = reNewline . Text.splitOn "\n"
        , joinLines = mconcat
        , stripIndent = Text.stripStart
        , endsWithNewline = ("\n" `Text.isSuffixOf`)
        }

htmlNewlines :: Newlines Html
htmlNewlines =
    Newlines
        { splitLines = fmap unsafeRawHtml . splitLines textNewlines . htmlSource
        , joinLines = unsafeRawHtml . joinLines textNewlines . fmap htmlSource
        , stripIndent = unsafeRawHtml . stripIndent textNewlines . htmlSource
        , endsWithNewline = endsWithNewline textNewlines . htmlSource
        }

-- | Helper; reinstates newlines after splitting a 'Text' into lines.
reNewline :: [Text] -> [Text]
reNewline [] = []
reNewline ("":[]) = []
reNewline (x:[]) = [x]
reNewline (x:"":[]) = [x <> "\n"]
reNewline (x:xs) = (x <> "\n") : reNewline xs

data RunState p m h
    = RunState
        { rsScope :: HashMap VarName (GVal (Run p m h))
        , rsCapture :: h
        , rsCurrentTemplate :: Template p -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
        , rsIndentation :: Maybe [h] -- current indentation level, if any
        , rsAtLineStart :: Bool -- is the next output position the first column
        , rsCurrentSourcePos :: p
        }

-- | Hoist a 'RunState' onto a different output type.
-- You don't normally need to use this directly; see 'hoistRun' and/or
-- 'hoistContext'.
hoistRunState :: Monad m => (h -> t) -> (t -> h) -> RunState p m h -> RunState p m t
hoistRunState fwd rev rs =
    RunState
        { rsScope = marshalGValEx (hoistRun fwd rev) (hoistRun rev fwd) <$> rsScope rs
        , rsCapture = fwd $ rsCapture rs
        , rsCurrentTemplate = rsCurrentTemplate rs
        , rsCurrentBlockName = rsCurrentBlockName rs
        , rsIndentation = fmap fwd <$> rsIndentation rs
        , rsAtLineStart = rsAtLineStart rs
        , rsCurrentSourcePos = rsCurrentSourcePos rs
        }

-- | Internal type alias for our template-runner monad stack.
type Run p m h = StateT (RunState p m h) (ReaderT (GingerContext p m h) m)

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run p m h a
liftRun = lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run p m h b
liftRun2 f x = liftRun $ f x

-- | Hoist a 'Run' action onto a different output type.
-- @hoistRun fwd rev action@ hoists the @action@ from @Run p m h a@ to
-- @Run p m t a@, applying @fwd@ and @rev@ to convert between the output
-- types.
hoistRun :: Monad m => (h -> t) -> (t -> h) -> Run p m h a -> Run p m t a
hoistRun fwd rev action = do
    contextT <- ask
    let contextH = hoistContext rev fwd contextT
    stateT <- get
    let stateH = hoistRunState rev fwd stateT
    (x, stateH') <- lift . lift $ runReaderT (runStateT action stateH) contextH
    let stateT' = hoistRunState fwd rev stateH'
    put stateT'
    return x

warn :: (Monad m) => Text -> Run p m h ()
warn msg = do
    warnFn <- asks contextWarn
    warnFn msg

warnFromMaybe :: Monad m => Text -> a -> Maybe a -> Run p m h a
warnFromMaybe msg d Nothing = warn msg >> return d
warnFromMaybe _ d (Just x) = return x

