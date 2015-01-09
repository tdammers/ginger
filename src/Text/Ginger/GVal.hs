{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}

-- | GVal is a generic unitype value, representing the kind of values that
-- Ginger can understand.
--
-- Most of the types in this module are parametrized over an 'm' type, which
-- is the host monad for template execution, as passed to 'runGingerT'.
module Text.Ginger.GVal
where

import Prelude ( (.), ($), (==), (/=)
               , (+), (-), (*), (/), div
               , (>>=)
               , undefined, otherwise, id
               , Maybe (..)
               , Bool (..)
               , Either (..)
               , Char
               , Int
               , Integer
               , Double
               , Show, show
               , fromIntegral, floor
               , not
               )
import qualified Prelude
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Safe (readMay, atMay)
import Data.Monoid
import Data.Scientific ( Scientific
                       , floatingOrInteger
                       , toBoundedInteger
                       )
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector

import Text.Ginger.Html

-- | A function that can be called from within a template execution context.
type Function m = ([(Maybe Text, GVal m)] -> m (GVal m))

-- | Ginger value.
data GVal m =
    List [GVal m] | -- ^ List/array
    Object (HashMap Text (GVal m)) | -- ^ Key/value, unordered (dictionary)
    String Text | -- ^ Plain text (considered tainted)
    Html Html | -- ^ Pre-escaped HTML (considered \'safe\')
    Boolean Bool | -- ^ Boolean value
    Number Scientific |
    -- ^ A number. 'Scientific' is chosen as the type to represent numbers,
    -- because we are mostly dealing with display logic here, and humans tend
    -- to think in decimal numbers, which makes 2-based floating-point numbers
    -- a less intuitive choice.
    Null | -- ^ null value, also used to represent failed lookups and such.
    Function (Function m) -- ^ A callable function, filter, macro, block, ...

-- | Types that implement conversion to 'GVal'
class ToGVal m a where
    toGVal :: a -> GVal m

-- | Trivial instance for 'GVal' itself
instance ToGVal m (GVal m) where
    toGVal = id

-- | For convenience, 'Show' is implemented in a way that looks similar to
-- JavaScript / JSON
instance Show (GVal m) where
    show (List xs) = "[" <> (mconcat . List.intersperse ", " . Prelude.map show $ xs) <> "]"
    show (Object o) = "{" <> (mconcat . List.intersperse ", " $ [ show k <> ": " <> show v | (k, v) <- HashMap.toList o ]) <> "}"
    show (String v) = show v
    show (Html h) = show h
    show (Boolean b) = show b
    show (Number n) =
        case floatingOrInteger n :: Either Double Integer of
            Left x -> show n
            Right x -> show x
    show Null = "null"
    show (Function _) = "<<function>>"

-- | Converting to HTML hooks into the ToHtml instance for 'Text' for most tags.
-- Tags that have no obvious textual representation render as empty HTML.
instance ToHtml (GVal m) where
    toHtml (List xs) = mconcat . Prelude.map toHtml $ xs
    toHtml (Object o) = mconcat . Prelude.map toHtml . HashMap.elems $ o
    toHtml (String s) = toHtml s
    toHtml (Html h) = h
    toHtml (Number n) = toHtml . Text.pack . show $ Number n
    toHtml (Boolean False) = html ""
    toHtml (Boolean True) = html "1"
    toHtml _ = html ""

toText :: GVal m -> Text
toText (List xs) = mconcat . Prelude.map toText $ xs
toText (Object o) = mconcat . Prelude.map toText . HashMap.elems $ o
toText (String s) = s
toText (Html h) = htmlSource h -- TODO: find a better way.
toText (Number n) = Text.pack . show $ Number n
toText (Boolean False) = ""
toText (Boolean True) = "1"
toText _ = ""

-- | Treat a 'GVal' as a dictionary and look up a value by key.
-- If the value is not a dictionary, return 'Nothing'.
lookup :: Text -> GVal m -> Maybe (GVal m)
lookup k (Object o) = HashMap.lookup k o
lookup k _ = Nothing

-- | Treat a 'GVal' as a flat list and look up a value by index.
-- If the value is not a List, or if the index exceeds the list length,
-- return 'Nothing'.
lookupIndex :: Int -> GVal m -> Maybe (GVal m)
lookupIndex i (List xs) = atMay xs i
lookupIndex _ _ = Nothing

lookupLoose :: GVal m -> GVal m -> Maybe (GVal m)
lookupLoose k (Object o) =
    HashMap.lookup (toText k) o
lookupLoose i (List xs) = lookupIndex (fromMaybe 0 $ toInt i) (List xs)
lookupLoose _ _ = Nothing

-- | Treat a 'GVal' as a dictionary and list all the keys, with no particular
-- ordering.
keys :: GVal m -> [Text]
keys (Object o) = HashMap.keys o
keys _ = []

-- | List the keys for list-like values. For dictionaries, these are the
-- dictionary keys, in no particular order; for plain lists, they are
-- 0-based integer indexes.
iterKeys :: GVal m -> [GVal m]
iterKeys (Object o) = Prelude.map String . HashMap.keys $ o
iterKeys (List xs) = Prelude.map (Number . fromIntegral) [0..Prelude.length xs]
iterKeys _ = []

-- | Convert a 'GVal' to a list of 'GVal's. If the value is not list-like
-- (i.e., neither an 'Object' nor a 'List'), the empty list is returned.
toList :: GVal m -> [GVal m]
toList (List xs) = xs
toList (Object o) = HashMap.elems o
toList _ = []

-- | Convert a 'GVal' to a number.
--
-- * 'Number' is simply unwrapped.
--
-- * 'String' is fed through 'read', returning 'Nothing' if parsing failed.
--
-- * Boolean 'True' is returned as 1
--
-- * Anything else is considered \"not a number\" and thus converted to
-- 'Nothing'
--
toNumber :: GVal m -> Maybe Scientific
toNumber (Number n) = Just n
toNumber (String s) = readMay . Text.unpack $ s
toNumber (Boolean False) = Nothing
toNumber (Boolean True) = Just 1
toNumber _ = Nothing

-- | Convert a 'GVal' to an 'Int'.
-- The conversion will fail when the value is not numeric, and also if
-- it is too large to fit in an 'Int'.
toInt :: GVal m -> Maybe Int
toInt x = toNumber x >>= toBoundedInteger

-- | Loose cast to boolean.
--
-- Numeric zero, empty strings, empty lists, empty objects, 'Null', and boolean
-- 'False' are considered falsy, anything else (including functions) is
-- considered true-ish.
toBoolean :: GVal m -> Bool
toBoolean (Number n) = n /= 0
toBoolean (String s) = not $ Text.null s
toBoolean (List xs) = not $ List.null xs
toBoolean (Object o) = not $ HashMap.null o
toBoolean (Boolean b) = b
toBoolean (Function _) = True
toBoolean _ = False

-- | Dynamically cast to a function.
-- This yields 'Just' a 'Function' if the value is a function, 'Nothing' if
-- it's not.
toFunction :: GVal m -> Maybe (Function m)
toFunction (Function f) = Just f
toFunction _ = Nothing

-- | Convert Aeson 'Value's to 'GVal's over an arbitrary host monad. Because
-- JSON cannot represent functions, this conversion will never produce a
-- 'Function'.
instance ToGVal m JSON.Value where
    toGVal (JSON.Number n) = Number n
    toGVal (JSON.String s) = String s
    toGVal (JSON.Bool b) = Boolean b
    toGVal (JSON.Null) = Null
    toGVal (JSON.Array a) = List (List.map toGVal $ Vector.toList a)
    toGVal (JSON.Object o) = Object (HashMap.map toGVal $ o)

instance ToGVal m v => ToGVal m (Maybe v) where
    toGVal Nothing = Null
    toGVal (Just x) = toGVal x

instance ToGVal m v => ToGVal m [v] where
    toGVal = List . List.map toGVal

instance ToGVal m Bool where
    toGVal = Boolean

instance ToGVal m [Char] where
    toGVal = String . Text.pack

instance ToGVal m Text where
    toGVal = String
