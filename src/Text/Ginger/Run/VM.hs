{-#LANGUAGE FlexibleContexts #-}
module Text.Ginger.Run.VM
where

import Text.Ginger.Run.Type
import Text.Ginger.Run.FuncUtils
import Text.Ginger.AST
import Text.Ginger.Buildable
import Text.Ginger.GVal
import Data.Monoid ( (<>) )
import Control.Monad.State (MonadState (..), get, gets, modify)
import Control.Monad.Reader (asks, local)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

-- | Helper function to run a State action with a temporary state, reverting
-- to the old state after the action has finished.
withLocalState :: (Monad m, MonadState s m) => m a -> m a
withLocalState a = do
    s <- get
    r <- a
    put s
    return r

-- | Helper function to run a Scope action with a temporary scope, reverting
-- to the old scope after the action has finished.
withLocalScope :: (Monad m) => Run p m h a -> Run p m h a
withLocalScope a = do
    scope <- gets rsScope
    r <- a
    modify (\s -> s { rsScope = scope })
    return r

-- | Override the encoder used for converting 'GVal's to the output type.
-- This can be used for things like temporarily disabling HTML encoding.
withEncoder :: (ContextEncodable h, Monad m) => (GVal (Run p m h) -> h) -> Run p m h a -> Run p m h a
withEncoder encoder =
    local (\context -> context { contextEncode = encode })

setVar :: Monad m => VarName -> GVal (Run p m h) -> Run p m h ()
setVar name val = do
    vars <- gets rsScope
    let vars' = HashMap.insert name val vars
    modify (\s -> s { rsScope = vars' })

getVar :: Monad m => VarName -> Run p m h (GVal (Run p m h))
getVar key = do
    vars <- gets rsScope
    case HashMap.lookup key vars of
        Just val ->
            return val
        Nothing -> do
            l <- asks contextLookup
            l key

clearCapture :: (Buildable h, Monoid (Builder h), Monad m) => Run p m h ()
clearCapture = modify (\s -> s { rsCapture = mempty })

appendCapture :: (Buildable h, Monoid (Builder h), Monad m) => h -> Run p m h ()
appendCapture h = modify (\s -> s { rsCapture = rsCapture s <> toBuilder h })

fetchCapture :: (Buildable h, Monad m) => Run p m h h
fetchCapture = fromBuilder <$> gets rsCapture

