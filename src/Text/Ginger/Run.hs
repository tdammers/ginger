module Text.Ginger.Run
( runGingerM
, runGinger
, GingerContext
, GingerValue (..)
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

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader

-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m v
    = GingerContext
        { contextLookup :: VarName -> m v
        , contextWriteHtml :: Html -> m ()
        }

-- | Since Ginger is unityped, any type that you want to use as a value in
-- a template must implement a few common operations.
class ToHtml v => GingerValue v where
    lookup :: Text -> v -> Maybe v -- ^ used for dictionary-style access
    keys :: v -> [Text] -- ^ get all dictionary keys, if any
    toList :: v -> [v] -- ^ access as flat list
    toString :: v -> Text -- ^ access as string
    fromString :: Text -> v -- ^ create from string
    (~+~) :: v -> v -> v -- ^ loosely-typed addition
    (~-~) :: v -> v -> v -- ^ loosely-typed subtraction
    (~*~) :: v -> v -> v -- ^ loosely-typed multiplication
    (~/~) :: v -> v -> v -- ^ loosely-typed division
    (~//~) :: v -> v -> v -- ^ loosely-typed integer division
    (~~~) :: v -> v -> v -- ^ string concatenation
    stringly :: (Text -> Text) -> v -> v -- ^ "lift" a string transformation onto ginger values
    stringly f = fromString . f . toString

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
