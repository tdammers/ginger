-- | GingerValue instance for @Text@.
module Text.Ginger.Value.Text
where

import Text.Ginger.Value
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Scientific (Scientific)
import Data.Maybe ( fromMaybe
                  )
import Control.Applicative ( (<$>), (<*>)
                           )
import Safe (readMay)
import Data.Monoid ( (<>) )

instance GingerValue Text where
    lookup k v = Nothing
    keys = const []
    toList = const []
    toString = id
    fromString = id
    (~+~) = withScientificText (+)
    (~-~) = withScientificText (-)
    (~*~) = withScientificText (*)
    (~/~) = withScientificText (/)
    (~//~) = withIntegerText div
    (~~~) = (<>)
    stringly = id

withScientificText :: (Scientific -> Scientific -> Scientific) -> Text -> Text -> Text
withScientificText = withText

withIntegerText :: (Integer -> Integer -> Integer) -> Text -> Text -> Text
withIntegerText = withText

withText :: (Read a, Read b, Show c) => (a -> b -> c) -> Text -> Text -> Text
withText f a b =
    reText $ f <$> unText a <*> unText b
    where
        unText :: Read a => Text -> Maybe a
        unText = readMay . Text.unpack

        reText :: Show a => Maybe a -> Text
        reText Nothing = Text.empty
        reText (Just v) = Text.pack . show $ v
