{-#LANGUAGE OverloadedStrings #-}
-- | 'GingerValue' instance for Aeson 'Value's
module Text.Ginger.Value.JSON
where

import Text.Ginger.Html
import Text.Ginger.Value
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Scientific (Scientific)
import Data.Maybe ( fromMaybe
                  )
import Data.Aeson ( Value (..)
                  )
import Control.Applicative ( (<$>), (<*>)
                           )
import Safe (readMay)
import Data.Monoid ( (<>), mconcat )
import Data.List (intersperse)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

instance ToHtml Value where
    toHtml = html . toText

instance GingerValue Value where
    lookup k (Object items) = HashMap.lookup k items
    lookup _ _ = Nothing
    keys (Object items) = HashMap.keys items
    keys _ = []
    toList (Object items) = HashMap.elems items
    toList (Array items) = Vector.toList items
    toList _ = []
    toBoolean (Object items) = not $ HashMap.null items
    toBoolean (Array items) = not $ Vector.null items
    toBoolean (String str) = not $ Text.null str
    toBoolean (Number n) = n /= 0
    toBoolean (Bool b) = b
    toBoolean Null = False

    toNumber (Number n) = Just n
    toNumber (String str) = readMay . Text.unpack $ str
    toNumber (Bool True) = Just 1
    toNumber _ = Nothing
    
    (~+~) = numerically (+)
    (~-~) = numerically (-)
    (~*~) = numerically (*)
    (~/~) = numerically (/)
    (~//~) = integrally div
    (~~~) = stringly (<>)

numerically :: (Scientific -> Scientific -> Scientific) -> Value -> Value -> Value
numerically f a b = Number . fromMaybe 0 $ f <$> toNumber a <*> toNumber b

integrally :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
integrally f a b = Number . fromIntegral . fromMaybe 0 $ f <$> (floor <$> toNumber a) <*> (floor <$> toNumber b)

stringly :: (Text -> Text -> Text) -> Value -> Value -> Value
stringly f a b = String $ f (toText a) (toText b)

toText :: Value -> Text
toText (String s) = s
toText (Number n) = Text.pack . show $ n
toText (Array a) = mconcat . intersperse ", " . map toText . Vector.toList $ a
toText _ = ""
