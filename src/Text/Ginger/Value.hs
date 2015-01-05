-- | Defines the typeclass that Ginger values must implement for the template
-- runner to deal with them.
module Text.Ginger.Value
where

import Text.Ginger.Html
import Data.Text (Text)

-- | Since Ginger is unityped, any type that you want to use as a value in
-- a template must implement a few common operations.
class ToHtml v => GingerValue v where
    lookup :: Text -> v -> Maybe v -- ^ used for dictionary-style access
    keys :: v -> [Text] -- ^ get all dictionary keys, if any
    toList :: v -> [v] -- ^ access as flat list
    toString :: v -> Text -- ^ access as string
    toBoolean :: v -> Bool -- ^ cast to boolean
    fromString :: Text -> v -- ^ create from string
    (~+~) :: v -> v -> v -- ^ loosely-typed addition
    (~-~) :: v -> v -> v -- ^ loosely-typed subtraction
    (~*~) :: v -> v -> v -- ^ loosely-typed multiplication
    (~/~) :: v -> v -> v -- ^ loosely-typed division
    (~//~) :: v -> v -> v -- ^ loosely-typed integer division
    (~~~) :: v -> v -> v -- ^ string concatenation
    stringly :: (Text -> Text) -> v -> v -- ^ "lift" a string transformation onto ginger values
    stringly f = fromString . f . toString


