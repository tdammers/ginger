module Text.Ginger.Run
( runGinger
)
where

import Text.Ginger.AST
import Text.Ginger.Html

import Data.Text (Text)
import qualified Data.Text as Text

data GingerContext m v
    = GingerContext
        { contextLookup :: VarName -> m v
        }

runGinger :: (Monad m, ToHtml v) => GingerContext m v -> Template -> m ()
runGinger = undefined
