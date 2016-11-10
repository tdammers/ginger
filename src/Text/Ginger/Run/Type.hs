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
, liftRun
, liftRun2
, Run (..)
, RunState (..)
, RuntimeError (..)
, runtimeErrorWhat
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
        }

contextWriteEncoded :: GingerContext m h -> GVal (Run m h) -> Run m h ()
contextWriteEncoded context =
    contextWrite context . contextEncode context

-- | Create an execution context for runGingerT.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM' :: (Monad m, Functor m)
             => (VarName -> Run m h (GVal (Run m h)))
             -> (h -> m ())
             -> (GVal (Run m h) -> h)
             -> GingerContext m h
makeContextM' lookupFn writeFn encodeFn =
    GingerContext
        { contextLookup = lookupFn
        , contextWrite = liftRun2 writeFn
        , contextEncode = encodeFn
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
            -> GingerContext (Writer h) h
makeContext' lookupFn encodeFn =
    makeContextM'
        (return . lookupFn)
        tell
        encodeFn

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
makeContextHtml l = makeContext' l toHtml

makeContextHtmlM :: (Monad m, Functor m)
                 => (VarName -> Run m Html (GVal (Run m Html)))
                 -> (Html -> m ())
                 -> GingerContext m Html
makeContextHtmlM l w = makeContextM' l w toHtml

makeContextText :: (VarName -> GVal (Run (Writer Text) Text))
                -> GingerContext (Writer Text) Text
makeContextText l = makeContext' l asText

makeContextTextM :: (Monad m, Functor m)
                 => (VarName -> Run m Text (GVal (Run m Text)))
                 -> (Text -> m ())
                 -> GingerContext m Text
makeContextTextM l w = makeContextM' l w asText


data RunState m h
    = RunState
        { rsScope :: HashMap VarName (GVal (Run m h))
        , rsCapture :: h
        , rsCurrentTemplate :: Template -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
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


