-- | Execute Ginger templates in an arbitrary monad.
module Text.Ginger.Run
( runGingerM
, runGinger
, GingerContext
, GingerValue (..)
, CallArgs (..)
, makeContext
, makeContextM
)
where

import Prelude ( (.), ($)
               , undefined
               , Maybe (..)
               )
import qualified Prelude
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.Value

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m v
    = GingerContext
        { contextLookup :: VarName -> m v
        , contextWriteHtml :: Html -> m ()
        }

-- | Wrapper structure to hold function call arguments.
data CallArgs v =
    CallArgs
        { namedArgs :: HashMap Text v -- ^ Argument given by declared name
        , kwargs :: HashMap Text v -- ^ Undeclared named arguments
        , args :: [v] -- ^ Excess positional arguments
        }

-- | Create an execution context for runGingerM.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM :: (Monad m, GingerValue v) => (VarName -> m v) -> (Html -> m ()) -> GingerContext m v
makeContextM = GingerContext

-- | Create an execution context for runGinger.
-- The argument is a lookup function that maps top-level context keys to ginger
-- values.
makeContext :: GingerValue v => (VarName -> v) -> GingerContext (Writer Html) v
makeContext l = makeContextM (return . l) tell

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad,
-- the @v@ parameter is the type for Ginger values.
runGingerM :: (Monad m, GingerValue v) => GingerContext m v -> Template -> m ()
runGingerM = undefined

-- | Purely expand a Ginger template. @v@ is the type for Ginger values.
runGinger :: (GingerValue v) => GingerContext (Writer Html) v -> Template -> Html
runGinger context template = execWriter $ runGingerM context template
