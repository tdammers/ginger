{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
-- | Execute Ginger templates in an arbitrary monad.
module Text.Ginger.Run
( runGingerT
, runGinger
, GingerContext
, makeContext
, makeContextM
, Run, liftRun, liftRun2
)
where

import Prelude ( (.), ($), (==), (/=)
               , (+), (-), (*), (/), div
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , fromIntegral, floor
               , not
               , show
               , uncurry
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
import Control.Monad.State
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Safe (readMay)

-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m
    = GingerContext
        { contextLookup :: VarName -> Run m (GVal (Run m))
        , contextWriteHtml :: Html -> Run m ()
        }

data RunState m
    = RunState
        { rsScope :: HashMap VarName (GVal (Run m))
        , rsCapture :: Html
        }

defRunState :: Monad m => RunState m
defRunState =
    RunState
        { rsScope = HashMap.fromList [ ("raw", Function gfnRawHtml) ]
        , rsCapture = html ""
        }
    where
        gfnRawHtml [] = return Null
        gfnRawHtml ((Nothing, v):_) = return . Html . unsafeRawHtml . toText $ v

-- | Create an execution context for runGingerT.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM :: (Monad m, Functor m) => (VarName -> Run m (GVal (Run m))) -> (Html -> m ()) -> GingerContext m
makeContextM l w = GingerContext l (liftRun2 w)

liftLookup :: (Monad m, ToGVal (Run m) v) => (VarName -> m v) -> VarName -> Run m (GVal (Run m))
liftLookup f k = do
    v <- liftRun $ f k
    return . toGVal $ v

-- | Create an execution context for runGinger.
-- The argument is a lookup function that maps top-level context keys to ginger
-- values.
makeContext :: (ToGVal (Run (Writer Html)) v) => (VarName -> GVal (Run (Writer Html))) -> GingerContext (Writer Html)
makeContext l =
    makeContextM
        (return . l)
        tell

-- | Purely expand a Ginger template. @v@ is the type for Ginger values.
runGinger :: GingerContext (Writer Html) -> Template -> Html
runGinger context template = execWriter $ runGingerT context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad,
-- the @v@ parameter is the type for Ginger values.
runGingerT :: (Monad m, Functor m) => GingerContext m -> Template -> m ()
runGingerT context tpl = runReaderT (evalStateT (runTemplate tpl) defRunState) context

-- | Internal type alias for our template-runner monad stack.
type Run m = StateT (RunState m) (ReaderT (GingerContext m) m)

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run m a
liftRun = lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run m b
liftRun2 f x = liftRun $ f x

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

runStatement (SetVarS name valExpr) = do
    val <- runExpression valExpr
    setVar name val

runStatement (DefMacroS name macro) = do
    let val = macroToGVal macro
    setVar name val

runStatement (ScopedS body) = withLocalState $ runStatement body

runStatement (ForS varNameIndex varNameValue itereeExpr body) = do
    iteree <- runExpression itereeExpr
    let values = toList iteree
        indexes = iterKeys iteree
    sequence_ (Prelude.zipWith iteration indexes values)
    where
        iteration index value = withLocalState $ do
            setVar varNameValue value
            case varNameIndex of
                Nothing -> return ()
                Just n -> setVar n index
            runStatement body

-- | Deeply magical function that converts a 'Macro' into a Function.
macroToGVal :: (Functor m, Monad m) => Macro -> GVal (Run m)
macroToGVal (Macro argNames body) =
    Function $ \args -> (
        -- Establish a local state to not contaminate the parent scope
        -- with function arguments and local variables
        withLocalState $ do
            -- Establish a local context, where we override the HTML writer,
            -- rewiring it to append any output to the state's capture.
            local (\c -> c { contextWriteHtml = appendCapture }) $ do
                -- TODO: import args into local scope
                let (matchedArgs, positionalArgs, namedArgs) = matchFuncArgs argNames args
                forM (HashMap.toList matchedArgs) (uncurry setVar)
                setVar "varargs" $ List positionalArgs
                setVar "kwargs" $ Object namedArgs
                runStatement body
                -- At this point, we're still inside the local state, so the
                -- capture contains the macro's output; we now simply return
                -- the capture as the function's return value.
                Html <$> fetchCapture)

-- | Helper function to run a State action with a temporary state, reverting
-- to the old state after the action has finished.
withLocalState :: (Monad m, MonadState s m) => m a -> m a
withLocalState a = do
    s <- get
    r <- a
    put s
    return r

setVar :: Monad m => VarName -> GVal (Run m) -> Run m ()
setVar name val = do
    vars <- gets rsScope
    let vars' = HashMap.insert name val vars
    modify (\s -> s { rsScope = vars' })

getVar :: Monad m => VarName -> Run m (GVal (Run m))
getVar key = do
    vars <- gets rsScope
    case HashMap.lookup key vars of
        Just val ->
            return val
        Nothing -> do
            l <- asks contextLookup
            l key

appendCapture :: Monad m => Html -> Run m ()
appendCapture h = modify (\s -> s { rsCapture = rsCapture s <> h })

fetchCapture :: Monad m => Run m Html
fetchCapture = gets rsCapture

-- | Run (evaluate) an expression and return its value into the Run monad
runExpression :: (Monad m, Functor m) => Expression -> Run m (GVal (Run m))
runExpression (StringLiteralE str) = return . String $ str
runExpression (NumberLiteralE n) = return . Number $ n
runExpression (BoolLiteralE b) = return . Boolean $ b
runExpression (NullLiteralE) = return Null
runExpression (VarE key) = getVar key
runExpression (ListE xs) = List <$> forM xs runExpression
runExpression (ObjectE xs) = do
    items <- forM xs $ \(a, b) -> do
        l <- toText <$> runExpression a
        r <- runExpression b
        return (l, r)
    return . Object . HashMap.fromList $ items
runExpression (MemberLookupE baseExpr indexExpr) = do
    base <- runExpression baseExpr
    index <- runExpression indexExpr
    return . fromMaybe Null . lookupLoose index $ base
runExpression (CallE funcE argsEs) = do
    args <- forM argsEs $
        \(argName, argE) -> (argName,) <$> runExpression argE
    func <- toFunction <$> runExpression funcE
    case func of
        Nothing -> return Null
        Just f -> f args

-- | Helper function to output a HTML value using whatever print function the
-- context provides.
echo :: (Monad m, Functor m, ToHtml h) => h -> Run m ()
echo src = do
    p <- asks contextWriteHtml
    p (toHtml src)
