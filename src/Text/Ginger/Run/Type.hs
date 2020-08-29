{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | The internals of the 'Run' monad, and various things needed to make the
-- magic happen. You will not normally need to import this module;
-- 'Text.Ginger.Run' re-exports the things you probably want. However, if you
-- want to provide your own run monad that extends 'Run' somehow, this module
-- may be of use.
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
, RuntimeError (..)
, runtimeErrorWhat
, runtimeErrorWhere
, runtimeErrorMessage
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
, throwHere
, withSourcePos
, getSourcePos
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
import Text.Ginger.Buildable
import Text.Ginger.Html
import Text.Ginger.GVal
import Text.Ginger.Parse (ParserError (..), sourceLine, sourceColumn, sourceName)
import Text.Printf
import Text.PrintfA
import Data.Scientific (formatScientific)
import Control.Monad.Except (ExceptT (..))
import Data.Default (Default (..), def)

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
import Control.Monad.Except
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
        , contextWarn :: RuntimeError p -> Run p m h ()
        , contextEncode :: GVal (Run p m h) -> h
        , contextNewlines :: Maybe (Newlines h)
        }

-- | Hoist a context onto a different output type.
-- @hoistContext fwd rev context@ returns a context over a different
-- output type, applying the @fwd@ and @rev@ projections to convert
-- between the original and desired output types.
hoistContext :: (Monad m, Buildable h, Buildable t)
             => (h -> t) -> (t -> h) -> GingerContext p m h -> GingerContext p m t
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
              -> (RuntimeError p -> m ())
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
makeContextM' :: Monad m
             => (VarName -> Run p m h (GVal (Run p m h)))
             -> (h -> m ())
             -> (GVal (Run p m h) -> h)
             -> Maybe (Newlines h)
             -> GingerContext p m h
makeContextM' lookupFn writeFn encodeFn newlines =
  makeContextExM' lookupFn writeFn (Prelude.const $ return ()) encodeFn newlines

makeContextExM' :: Monad m
             => (VarName -> Run p m h (GVal (Run p m h)))
             -> (h -> m ())
             -> (RuntimeError p -> m ())
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
makeContextM :: Monad m
             => (VarName -> Run p m Html (GVal (Run p m Html)))
             -> (Html -> m ())
             -> GingerContext p m Html
makeContextM = makeContextHtmlM

makeContextHtml :: (VarName -> GVal (Run p (Writer Html) Html))
                -> GingerContext p (Writer Html) Html
makeContextHtml l = makeContext' l toHtml (Just htmlNewlines)

makeContextHtmlM :: Monad m
                 => (VarName -> Run p m Html (GVal (Run p m Html)))
                 -> (Html -> m ())
                 -> GingerContext p m Html
makeContextHtmlM l w = makeContextM' l w toHtml (Just htmlNewlines)

makeContextHtmlExM :: Monad m
                 => (VarName -> Run p m Html (GVal (Run p m Html)))
                 -> (Html -> m ())
                 -> (RuntimeError p -> m ())
                 -> GingerContext p m Html
makeContextHtmlExM l w warn = makeContextExM' l w warn toHtml (Just htmlNewlines)

makeContextText :: (VarName -> GVal (Run p (Writer Text) Text))
                -> GingerContext p (Writer Text) Text
makeContextText l = makeContext' l asText (Just textNewlines)

makeContextTextM :: Monad m
                 => (VarName -> Run p m Text (GVal (Run p m Text)))
                 -> (Text -> m ())
                 -> GingerContext p m Text
makeContextTextM l w = makeContextM' l w asText (Just textNewlines)

makeContextTextExM :: Monad m
                 => (VarName -> Run p m Text (GVal (Run p m Text)))
                 -> (Text -> m ())
                 -> (RuntimeError p -> m ())
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
        , rsCapture :: Builder h
        , rsCurrentTemplate :: Template p -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
        , rsIndentation :: Maybe [h] -- current indentation level, if any
        , rsAtLineStart :: Bool -- is the next output position the first column
        , rsCurrentSourcePos :: p
        }

-- | Hoist a 'RunState' onto a different output type.
-- You don't normally need to use this directly; see 'hoistRun' and/or
-- 'hoistContext'.
hoistRunState :: (Monad m, Buildable h, Buildable t)
              => (h -> t) -> (t -> h) -> RunState p m h -> RunState p m t
hoistRunState fwd rev rs =
    RunState
        { rsScope = marshalGValEx (hoistRun fwd rev) (hoistRun rev fwd) <$> rsScope rs
        , rsCapture = toBuilder . fwd . fromBuilder $ rsCapture rs
        , rsCurrentTemplate = rsCurrentTemplate rs
        , rsCurrentBlockName = rsCurrentBlockName rs
        , rsIndentation = fmap fwd <$> rsIndentation rs
        , rsAtLineStart = rsAtLineStart rs
        , rsCurrentSourcePos = rsCurrentSourcePos rs
        }

data RuntimeError p = RuntimeError Text -- ^ Generic runtime error
                    | UndefinedBlockError Text -- ^ Tried to use a block that isn't defined
                    -- | Invalid arguments to function (function name, explanation)
                    | ArgumentsError (Maybe Text) Text
                    -- | Wrong type, expected one of...
                    | TypeError [Text] (Maybe Text)
                    -- | Invalid index
                    | IndexError Text
                    | EvalParseError ParserError
                    | NotAFunctionError
                    | RuntimeErrorAt p (RuntimeError p)
        deriving (Show)

instance Default (RuntimeError p) where
    def = RuntimeError ""

instance ToGVal m p => ToGVal m (RuntimeError p) where
    toGVal = runtimeErrorToGVal

runtimeErrorWhat :: RuntimeError p -> Text
runtimeErrorWhat (ArgumentsError funcName explanation) = "ArgumentsError"
runtimeErrorWhat (EvalParseError e) = "EvalParseError"
runtimeErrorWhat (RuntimeError msg) = "RuntimeError"
runtimeErrorWhat (UndefinedBlockError blockName) = "UndefinedBlockError"
runtimeErrorWhat NotAFunctionError = "NotAFunctionError"
runtimeErrorWhat (IndexError _) = "IndexError"
runtimeErrorWhat (TypeError _ _) = "TypeError"
runtimeErrorWhat (RuntimeErrorAt _ e) = runtimeErrorWhat e

runtimeErrorMessage :: RuntimeError p -> Text
runtimeErrorMessage (ArgumentsError Nothing explanation) =
    "invalid arguments: " <> explanation
runtimeErrorMessage (ArgumentsError (Just funcName) explanation) =
    "invalid arguments to function '" <> funcName <> "': " <> explanation
runtimeErrorMessage (TypeError expected actual) =
    "wrong type"
    <> case expected of
        [] -> ""
        [x] -> ", expected " <> x
        xs -> ", expected " <> (mconcat . List.intersperse " or " $ xs)
    <> case actual of
        Nothing -> ""
        Just x -> ", found " <> x
runtimeErrorMessage (IndexError i) =
    "invalid index " <> i
runtimeErrorMessage (EvalParseError e) =
    "parser error in eval()-ed code: " <> Text.pack (peErrorMessage e)
runtimeErrorMessage (RuntimeError msg) =
    msg
runtimeErrorMessage (UndefinedBlockError blockName) =
    "undefined block: '" <> blockName <> "'"
runtimeErrorMessage NotAFunctionError =
    "attempted to call something that is not a function"
runtimeErrorMessage (RuntimeErrorAt _ e) =
    runtimeErrorMessage e

runtimeErrorWhere :: RuntimeError p -> [p]
runtimeErrorWhere (RuntimeErrorAt p e) = p:runtimeErrorWhere e
runtimeErrorWhere _ = []

runtimeErrorToGVal :: forall m p. ToGVal m p => RuntimeError p -> GVal m
runtimeErrorToGVal e =
    let (callStack, props) = runtimeErrorToGValRaw e
        props' = (("callStack" :: Text) ~> callStack):props
    in (dict props') { asText = runtimeErrorMessage e }

runtimeErrorToGValRaw :: RuntimeError p -> ([p], [(Text, GVal m)])
runtimeErrorToGValRaw (RuntimeError msg) =
    ( []
    , rteGVal "RuntimeError" []
    )
runtimeErrorToGValRaw (UndefinedBlockError blockName) =
    ( []
    , rteGVal "UndefinedBlockError"
        [ "block" ~> blockName
        ]
    )
runtimeErrorToGValRaw (ArgumentsError funcName explanation) =
    ( []
    , rteGVal "ArgumentsError"
        [ "explanation" ~> explanation
        , "function" ~> funcName
        ]
    )
runtimeErrorToGValRaw (TypeError expected Nothing) =
    ( []
    , rteGVal "ArgumentsError"
        [ "expected" ~> expected
        ]
    )
runtimeErrorToGValRaw (TypeError expected (Just actual)) =
    ( []
    , rteGVal "ArgumentsError"
        [ "expected" ~> expected
        , "actual" ~> actual
        ]
    )
runtimeErrorToGValRaw (EvalParseError e) =
    ( []
    , rteGVal "EvalParseError"
        [ "errorMessage" ~> peErrorMessage e
        -- , "sourcePosition" ~> peSourcePosition e
        ]
    )
runtimeErrorToGValRaw NotAFunctionError =
    ( []
    , rteGVal "NotAFunctionError"
        []
    )

runtimeErrorToGValRaw (RuntimeErrorAt p e) =
    let (callStack, inner) = runtimeErrorToGValRaw e
    in (p:callStack, inner)

rteGVal :: Text -> [(Text, GVal m)] -> [(Text, GVal m)]
rteGVal what extra =
    ( [ "what" ~> what
      ]
      ++ extra
    )

-- | Internal type alias for our template-runner monad stack.
type Run p m h =
  ExceptT (RuntimeError p) (StateT (RunState p m h) (ReaderT (GingerContext p m h) m))

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run p m h a
liftRun = lift . lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run p m h b
liftRun2 f x = liftRun $ f x

-- | Hoist a 'Run' action onto a different output type.
-- @hoistRun fwd rev action@ hoists the @action@ from @Run p m h a@ to
-- @Run p m t a@, applying @fwd@ and @rev@ to convert between the output
-- types.
hoistRun :: (Monad m, Buildable h, Buildable t)
         => (h -> t) -> (t -> h) -> Run p m h a -> Run p m t a
hoistRun fwd rev action = do
    contextT <- ask
    let contextH = hoistContext rev fwd contextT
    stateT <- get
    let stateH = hoistRunState rev fwd stateT
    (x, stateH') <- lift . lift . lift $ runReaderT (runStateT (runExceptT action) stateH) contextH
    let stateT' = hoistRunState fwd rev stateH'
    put stateT'
    Prelude.either throwError return x

warn :: (Monad m) => RuntimeError p -> Run p m h ()
warn err = do
    pos <- getSourcePos
    warnFn <- asks contextWarn
    warnFn $ RuntimeErrorAt pos err

warnFromMaybe :: Monad m => RuntimeError p -> a -> Maybe a -> Run p m h a
warnFromMaybe err d Nothing = warn err >> return d
warnFromMaybe _ d (Just x) = return x

setSourcePos :: Monad m
             => p
             -> Run p m h ()
setSourcePos pos =
  modify (\s -> s { rsCurrentSourcePos = pos })

getSourcePos :: Monad m
             => Run p m h p
getSourcePos = gets rsCurrentSourcePos

throwHere :: Monad m => RuntimeError p -> Run p m h a
throwHere err = do
    pos <- getSourcePos
    throwError $ RuntimeErrorAt pos err

-- | @withSourcePos pos action@ runs @action@ in a context where the
-- current source location is set to @pos@. The original source position is
-- restored when @action@ finishes.
withSourcePos :: Monad m
              => p
              -> Run p m h a
              -> Run p m h a
withSourcePos pos a = do
  oldPos <- getSourcePos
  catchError
    (setSourcePos pos *> a <* setSourcePos oldPos)
    (\err -> throwError $ RuntimeErrorAt oldPos err)

