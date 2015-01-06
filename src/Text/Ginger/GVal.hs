{-#LANGUAGE OverloadedStrings #-}
-- | GVal is a generic unitype value, parametrized over a user type.
-- It implements 'GingerValue' semantics such that wrapped 'UserValue's are
-- preserved as much as possible, promoting to specific other constructors
-- when needed.
module Text.Ginger.GVal
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
import Data.Text (Text)
import qualified Data.Text as Text
import Safe (readMay)
import Data.Monoid
import Data.Scientific (Scientific)
import Control.Applicative

import Text.Ginger.Value
import Text.Ginger.Html

data GVal m v =
    UserValue v |
    List [GVal m v] |
    -- Object (HashMap Text (GVal m v)) |
    String Text |
    Html Html |
    Boolean Bool |
    Number Scientific |
    Function ([(Maybe Text, GVal m v)] -> m (GVal m v))

-- | Forward 'UserValue' to the user type, implement in a somewhat useful 
-- way for the others.
instance ToHtml v => ToHtml (GVal m v) where
    toHtml (UserValue v) = toHtml v
    toHtml (List xs) = mconcat . Prelude.map toHtml $ xs
    toHtml (String s) = toHtml s
    toHtml (Html h) = h
    toHtml (Boolean False) = html ""
    toHtml (Boolean True) = html "1"
    toHtml (Function _) = html ""

instance GingerValue v => GingerValue (GVal m v) where
    -- | Forward lookups to wrapped user values
    lookup key (UserValue v) = UserValue <$> lookup key v
    -- | Other constructors do not support key lookup
    lookup key _ = Nothing

    -- | For wrapped user values, forward the call
    keys (UserValue v) = keys v
    -- | Other constructors do not provide keys
    keys _ = []

    -- | For wrapped user values, forward the call
    toList (UserValue v) = Prelude.map UserValue $ toList v
    -- | Lists can be trivially converted
    toList (List xs) = xs
    -- | Other values cannot be converted to lists
    toList _ = []

    -- | Forward for wrapped user values
    toBoolean (UserValue v) = toBoolean v
    -- | Loose conversion for other constructors
    toBoolean (List xs) = Prelude.null xs
    toBoolean (String s) = Text.null s
    toBoolean (Number n) = n /= 0
    toBoolean (Html h) = Text.null . htmlSource $ h
    toBoolean (Boolean b) = b
    toBoolean (Function _) = True

    toNumber (UserValue v) = toNumber v
    toNumber (Number n) = Just n
    toNumber (String s) = readMay . Text.unpack $ s
    toNumber (Html h) = readMay . Text.unpack . htmlSource $ h
    toNumber (Boolean True) = Just 1
    toNumber _ = Nothing

    -- Numeric operations keep UserValues intact where possible, and promote
    -- to Number otherwise.
    UserValue a ~+~ UserValue b = UserValue (a ~+~ b)
    a ~+~ b = Number $ (fromMaybe 0 . toNumber) a + (fromMaybe 0 . toNumber) b
    UserValue a ~-~ UserValue b = UserValue (a ~-~ b)
    a ~-~ b = Number $ (fromMaybe 0 . toNumber) a - (fromMaybe 0 . toNumber) b
    UserValue a ~*~ UserValue b = UserValue (a ~*~ b)
    a ~*~ b = Number $ (fromMaybe 0 . toNumber) a * (fromMaybe 0 . toNumber) b
    UserValue a ~/~ UserValue b = UserValue (a ~/~ b)
    a ~/~ b = Number $ (fromMaybe 0 . toNumber) a / (fromMaybe 0 . toNumber) b
    UserValue a ~//~ UserValue b = UserValue (a ~//~ b)
    a ~//~ b = Number . fromIntegral $ floor ((fromMaybe 0 . toNumber) a) `div` floor ((fromMaybe 0 . toNumber) b)

    -- UserValues are forwarded
    UserValue a ~~~ UserValue b = UserValue (a ~~~ b)
    -- Strings can be concatenated as-is
    String a ~~~ String b = String (a <> b)
    -- If either value is HTML, the result must also be HTML
    Html h ~~~ b = Html $ h <> toHtml b
    a ~~~ Html h = Html $ toHtml a <> h
    a ~~~ b
        | not (toBoolean b) = a
        | not (toBoolean a) = b
        | otherwise = Html (toHtml a <> toHtml b)


