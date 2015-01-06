{-#LANGUAGE OverloadedStrings #-}
-- | Execute Ginger templates in an arbitrary monad.
module Text.Ginger.Run
( runGingerM
, runGinger
, GingerContext
, module Text.Ginger.Value
, CallArgs (..)
, makeContext
, makeContextM
)
where

import Prelude ( (.), ($), (==)
               , undefined, otherwise
               , Maybe (..)
               )
import qualified Prelude
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.Value

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

data GVal m v =
    UserValue v |
    List [GVal m v] |
    -- Object (HashMap Text (GVal m v)) |
    String Text |
    Html Html |
    Function ([(Maybe Text, GVal m v)] -> m (GVal m v))

instance ToHtml v => ToHtml (GVal m v) where
    toHtml (UserValue v) = toHtml v
    toHtml (List xs) = mconcat . Prelude.map toHtml $ xs
    toHtml (String s) = toHtml s
    toHtml (Html h) = h
    toHtml (Function _) = toHtml ""

instance GingerValue v => GingerValue (GVal m v) where


-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m v
    = GingerContext
        { contextLookup :: VarName -> m (GVal m v)
        , contextWriteHtml :: Html -> m ()
        }

-- | Create an execution context for runGingerM.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM :: (Monad m, Functor m, GingerValue v) => (VarName -> m v) -> (Html -> m ()) -> GingerContext m v
makeContextM l w = GingerContext (liftLookup l) w

liftLookup :: Monad m => (VarName -> v) -> (VarName -> GVal m v)
liftLookup f k = do
    v <- f k
    return . UserValue $ v

-- | Create an execution context for runGinger.
-- The argument is a lookup function that maps top-level context keys to ginger
-- values.
makeContext :: GingerValue v => (VarName -> v) -> GingerContext (Writer Html) v
makeContext l = makeContextM (return . l) tell

-- | Purely expand a Ginger template. @v@ is the type for Ginger values.
runGinger :: (GingerValue v) => GingerContext (Writer Html) v -> Template -> Html
runGinger context template = execWriter $ runGingerM context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad,
-- the @v@ parameter is the type for Ginger values.
runGingerM :: (Monad m, Functor m, GingerValue v) => GingerContext m v -> Template -> m ()
runGingerM context tpl = runReaderT (runTemplate tpl) context

-- | Internal type alias for our template-runner monad stack.
type Run m v = ReaderT (GingerContext m v) m

-- | Run a template.
runTemplate :: (Monad m, Functor m, GingerValue v) => Template -> Run m v ()
runTemplate = runStatement . templateBody

-- | Run one statement.
runStatement :: (Monad m, Functor m, GingerValue v) => Statement -> Run m v ()
runStatement (MultiS xs) = forM_ xs runStatement
runStatement (LiteralS html) = echo html
runStatement (InterpolationS expr) = runExpression expr >>= echo
runStatement (IfS condExpr true false) = do
    cond <- runExpression condExpr
    runStatement $ if toBoolean cond then true else false

runStatement (ForS varName itereeExpr body) = do
    iteree <- toList <$> runExpression itereeExpr
    forM_ iteree $ \iterator -> do
        parentLookup <- asks contextLookup
        let localLookup k
                | k == varName = return iterator
                | otherwise = parentLookup k
        local
            (\c -> c { contextLookup = localLookup })
            (runStatement body)

-- | Run (evaluate) an expression and return its value into the Run monad
runExpression :: (Monad m, Functor m, GingerValue v) => Expression -> Run m v v
runExpression (StringLiteralE str) = return $ fromString str
runExpression (VarE key) = do
    l <- asks contextLookup
    lift $ l key

-- | Helper function to output a HTML value using whatever print function the
-- context provides.
echo :: (Monad m, Functor m, GingerValue v, ToHtml h) => h -> Run m v ()
echo src = do
    p <- asks contextWriteHtml
    lift $ p (toHtml src)
