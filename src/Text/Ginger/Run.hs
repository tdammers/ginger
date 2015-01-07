{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE OverloadedStrings #-}
-- | Execute Ginger templates in an arbitrary monad.
module Text.Ginger.Run
( runGingerT
, runGinger
, GingerContext
, makeContext
, makeContextM
)
where

import Prelude ( (.), ($), (==), (/=)
               , (+), (-), (*), (/), div
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , fromIntegral, floor
               , not
               )
import qualified Prelude
import Data.Maybe (fromMaybe)
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.GVal

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Safe (readMay)

-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m
    = GingerContext
        { contextLookup :: VarName -> m (GVal m)
        , contextWriteHtml :: Html -> m ()
        }

-- | Create an execution context for runGingerT.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM :: (Monad m, Functor m, ToGVal m v) => (VarName -> m v) -> (Html -> m ()) -> GingerContext m
makeContextM l w = GingerContext (liftLookup l) w

liftLookup :: (Monad m, ToGVal m v) => (VarName -> m v) -> VarName -> m (GVal m)
liftLookup f k = do
    v <- f k
    return . toGVal $ v

-- | Create an execution context for runGinger.
-- The argument is a lookup function that maps top-level context keys to ginger
-- values.
makeContext :: (ToGVal (Writer Html) v) => (VarName -> v) -> GingerContext (Writer Html)
makeContext l = makeContextM (return . l) tell

-- | Purely expand a Ginger template. @v@ is the type for Ginger values.
runGinger :: GingerContext (Writer Html) -> Template -> Html
runGinger context template = execWriter $ runGingerT context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad,
-- the @v@ parameter is the type for Ginger values.
runGingerT :: (Monad m, Functor m) => GingerContext m -> Template -> m ()
runGingerT context tpl = runReaderT (runTemplate tpl) context

-- | Internal type alias for our template-runner monad stack.
type Run m = ReaderT (GingerContext m) m

-- | Run a template.
runTemplate :: (Monad m, Functor m) => Template -> Run m ()
runTemplate = runStatement . templateBody

-- | Run one statement.
runStatement :: (Monad m, Functor m) => Statement -> Run m ()
runStatement NullS = return ()
runStatement (MultiS xs) = forM_ xs runStatement
runStatement (LiteralS html) = echo html
runStatement (InterpolationS expr) = runExpression expr >>= echo
runStatement (IfS condExpr true false) = do
    cond <- runExpression condExpr
    runStatement $ if toBoolean cond then true else false

runStatement (ForS varNameIndex varNameValue itereeExpr body) = do
    iteree <- runExpression itereeExpr
    let values = toList iteree
        indexes = iterKeys iteree
    parentLookup <- asks contextLookup
    forM_ (Prelude.zip indexes values) $ \(index, value) -> do
        let localLookup k
                | k == varNameValue = return value
                | Just k == varNameIndex = return index
                | otherwise = parentLookup k
        local
            (\c -> c { contextLookup = localLookup })
            (runStatement body)

-- | Run (evaluate) an expression and return its value into the Run monad
runExpression :: (Monad m, Functor m) => Expression -> Run m (GVal m)
runExpression (StringLiteralE str) = return . String $ str
runExpression (NumberLiteralE n) = return . Number $ n
runExpression (VarE key) = do
    l <- asks contextLookup
    lift $ l key

-- | Helper function to output a HTML value using whatever print function the
-- context provides.
echo :: (Monad m, Functor m, ToHtml h) => h -> Run m ()
echo src = do
    p <- asks contextWriteHtml
    lift $ p (toHtml src)
