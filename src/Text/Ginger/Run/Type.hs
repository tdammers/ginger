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
, RuntimeError (..)
, runtimeErrorWhat
-- * The Newlines type
-- | Required for handling indentation
, Newlines (..)
-- * Hoisting
, hoistContext
, hoistRun
, hoistNewlines
, hoistRunState
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
import Text.Ginger.Parse (ParserError (..))
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
data GingerContext m h
    = GingerContext
        { contextLookup :: VarName -> Run m h (GVal (Run m h))
        , contextWrite :: h -> Run m h ()
        , contextEncode :: GVal (Run m h) -> h
        , contextNewlines :: Maybe (Newlines h)
        }

-- | Hoist a context onto a different output type.
-- @hoistContext fwd rev context@ returns a context over a different
-- output type, applying the @fwd@ and @rev@ projections to convert
-- between the original and desired output types.
hoistContext :: Monad m => (h -> t) -> (t -> h) -> GingerContext m h -> GingerContext m t
hoistContext fwd rev c =
    GingerContext
        { contextLookup = \varName ->
            marshalGValEx
                (hoistRun fwd rev)
                (hoistRun rev fwd) <$>
                hoistRun fwd rev (contextLookup c varName)
        , contextWrite = \val ->
            hoistRun fwd rev (contextWrite c $ rev val)
        , contextEncode = \gval ->
            fwd .
                contextEncode c .
                marshalGValEx (hoistRun rev fwd) (hoistRun fwd rev) $
                gval
        , contextNewlines =
            hoistNewlines fwd rev <$> contextNewlines c
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

data RunState m h
    = RunState
        { rsScope :: HashMap VarName (GVal (Run m h))
        , rsCapture :: h
        , rsCurrentTemplate :: Template -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
        , rsIndentation :: Maybe [h] -- current indentation level, if any
        , rsAtLineStart :: Bool -- is the next output position the first column
        }

-- | Hoist a 'RunState' onto a different output type.
-- You don't normally need to use this directly; see 'hoistRun' and/or
-- 'hoistContext'.
hoistRunState :: Monad m => (h -> t) -> (t -> h) -> RunState m h -> RunState m t
hoistRunState fwd rev rs =
    RunState
        { rsScope = marshalGValEx (hoistRun fwd rev) (hoistRun rev fwd) <$> rsScope rs
        , rsCapture = fwd $ rsCapture rs
        , rsCurrentTemplate = rsCurrentTemplate rs
        , rsCurrentBlockName = rsCurrentBlockName rs
        , rsIndentation = fmap fwd <$> rsIndentation rs
        , rsAtLineStart = rsAtLineStart rs
        }

data RuntimeError = RuntimeError Text -- ^ Generic runtime error
                  | UndefinedBlockError Text -- ^ Tried to use a block that isn't defined
                  | ArgumentsError -- ^ Invalid arguments to function
                        Text -- ^ name of function being called
                        Text -- ^ explanation
                  | EvalParseError ParserError
                  | NotAFunctionError
        deriving (Show)

instance Default RuntimeError where
    def = RuntimeError ""

instance ToGVal m RuntimeError where
    toGVal = runtimeErrorToGVal

runtimeErrorWhat :: RuntimeError -> Text
runtimeErrorWhat (ArgumentsError funcName explanation) = "ArgumentsError"
runtimeErrorWhat (EvalParseError e) = "EvalParseError"
runtimeErrorWhat (RuntimeError msg) = "RuntimeError"
runtimeErrorWhat (UndefinedBlockError blockName) = "UndefinedBlockError"
runtimeErrorWhat NotAFunctionError = "NotAFunctionError"

runtimeErrorToGVal :: RuntimeError -> GVal m
runtimeErrorToGVal (RuntimeError msg) =
    rteGVal "RuntimeError"
        msg []
runtimeErrorToGVal (UndefinedBlockError blockName) =
    rteGVal "UndefinedBlockError"
        ("undefined block: '" <> blockName <> "'")
        [ "block" ~> blockName
        ]
runtimeErrorToGVal (ArgumentsError funcName explanation) =
    rteGVal "ArgumentsError"
        ("invalid arguments to function '" <> funcName <> "': " <> explanation)
        [ "explanation" ~> explanation
        , "function" ~> funcName
        ]
runtimeErrorToGVal (EvalParseError e) =
    rteGVal "EvalParseError"
        ("error parsing eval()-ed code: " <> Text.pack (peErrorMessage e))
        [ "errorMessage" ~> peErrorMessage e
        , "sourceFile" ~> peSourceName e
        , "line" ~> peSourceLine e
        , "col" ~> peSourceColumn e
        ]
runtimeErrorToGVal NotAFunctionError =
    rteGVal "NotAFunctionError"
        ("attempted to call something that is not a function")
        []

rteGVal :: Text -> Text -> [(Text, GVal m)] -> GVal m
rteGVal what msg extra =
    (dict $
        [ "what" ~> what
        , "message" ~> msg
        ] ++ extra) { asText = msg }

-- | Internal type alias for our template-runner monad stack.
type Run m h = ExceptT RuntimeError (StateT (RunState m h) (ReaderT (GingerContext m h) m))

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run m h a
liftRun = lift . lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run m h b
liftRun2 f x = liftRun $ f x

-- | Hoist a 'Run' action onto a different output type.
-- @hoistRun fwd rev action@ hoists the @action@ from @Run m h a@ to
-- @Run m t a@, applying @fwd@ and @rev@ to convert between the output
-- types.
hoistRun :: Monad m => (h -> t) -> (t -> h) -> Run m h a -> Run m t a
hoistRun fwd rev action = do
    contextT <- ask
    let contextH = hoistContext rev fwd contextT
    stateT <- get
    let stateH = hoistRunState rev fwd stateT
    (x, stateH') <- lift . lift . lift $ runReaderT (runStateT (runExceptT action) stateH) contextH
    let stateT' = hoistRunState fwd rev stateH'
    put stateT'
    Prelude.either throwError return x
