{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE RankNTypes #-}

-- | GVal is a generic unitype value, representing the kind of values that
-- Ginger can understand.
--
-- Most of the types in this module are parametrized over an 'm' type, which
-- is the host monad for template execution, as passed to 'runGingerT'. For
-- most kinds of values, 'm' is transparent, and in many cases a 'ToGVal'
-- instance can be written that works for all possible 'm'; the reason we need
-- to parametrize the values themselves over the carrier monad is because we
-- want to support impure functions, which requires access to the underlying
-- carrier monad (e.g. 'IO').
module Text.Ginger.GVal
where

import Prelude hiding (toInteger)
import Control.Monad.Fail (MonadFail)
import Data.Maybe ( fromMaybe, catMaybes, isJust, mapMaybe, listToMaybe )
import Data.Text (Text)
import Data.String (IsString, fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.List as List
import Safe (readMay, atMay)
import Data.Monoid
import Data.Scientific ( Scientific
                       , floatingOrInteger
                       , toBoundedInteger
                       , toRealFloat
                       , scientific
                       , coefficient
                       , base10Exponent
                       )
import Data.Fixed (Fixed (..), Pico)
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector as Vector
import Control.Monad ((<=<), forM, mapM)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Default (Default, def)
import Text.Printf
import Debug.Trace (trace)
import Data.Time ( Day (..)
                 , defaultTimeLocale
                 , toModifiedJulianDay
                 , formatTime
                 , toGregorian
                 , fromGregorian
                 , LocalTime (..)
                 , ZonedTime (..)
                 , TimeOfDay (..)
                 , TimeZone (..)
                 , TimeLocale (..)
                 )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LText

import Text.Ginger.Html

-- * The Ginger Value type
--
-- | A sum type designed as the unitype for the template language. Any
-- value referenced in a template, returned from within a template, or used
-- in a template context, will be a 'GVal'.
-- @m@, in most cases, should be a 'Monad'.
data GVal m
  = GNull
  | GList [GVal m]
  | GDict [Text] (Map Text (GVal m))
  | GHtml Html
  | GText Text
  | GBool Bool
  | GNumber Scientific
  | GBytes ByteString
  | GJSON JSON.Value
  | GFunction (Function m)
  | GMultiverse [GVal m]
    -- ^ Use this to provide multiple alternative representations of a value

data ConversionQuality
  = CQPathological
  | CQRaw
  | CQRichDerived
  | CQRichExplicit
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

asList :: GVal m -> Maybe [GVal m]
asList (GList items) = Just items
asList (GDict ks kv) = Just [ v | k <- ks, Just v <- [Map.lookup k kv] ]
asList (GJSON (JSON.Array items)) = Just . fmap GJSON . Vector.toList $ items
asList (GJSON (JSON.Object items)) = Just . fmap (GJSON . snd) . List.sort . HashMap.toList $ items
asList (GMultiverse xs) = listToMaybe . mapMaybe asList $ xs
asList _ = Nothing

asDictItems :: GVal m -> Maybe [(Text, GVal m)]
asDictItems (GList items) = Just (zip (map (Text.pack . show) [0..]) items)
asDictItems (GDict ks kv) = Just [ (k, v) | k <- ks, Just v <- [Map.lookup k kv] ]
asDictItems (GJSON (JSON.Array items)) = Just . zip (map (Text.pack . show) [0..]) . fmap GJSON . Vector.toList $ items
asDictItems (GJSON (JSON.Object items)) = Just . HashMap.toList . fmap GJSON $ items
asDictItems (GMultiverse xs) = listToMaybe . mapMaybe asDictItems $ xs
asDictItems _ = Nothing

asHashMap :: GVal m -> Maybe (HashMap Text (GVal m))
asHashMap (GJSON (JSON.Object items)) = Just $ fmap GJSON items
asHashMap (GDict _ items) = Just $ HashMap.fromList . Map.toList $ items
asHashMap g = HashMap.fromList <$> asDictItems g

asMap :: GVal m -> Maybe (Map Text (GVal m))
asMap (GDict _ items) = Just items
asMap g = Map.fromList <$> asDictItems g

asHtml :: GVal m -> Html
asHtml (GList xs) = mconcat . map asHtml $ xs
asHtml g@(GDict {}) = fromMaybe mempty $ mconcat . map asHtml <$> asList g
asHtml x = snd . asHtmlQ $ x

asHtmlQ :: GVal m -> (ConversionQuality, Html)
asHtmlQ GNull = (CQPathological, mempty)
asHtmlQ (GList xs) = (CQRaw, mconcat . map asHtml $ xs)
asHtmlQ g@(GDict {}) = (CQRaw, fromMaybe mempty $ mconcat . map asHtml <$> asList g)
asHtmlQ (GJSON (JSON.Array items)) = (CQRaw, Vector.foldl' (<>) mempty . fmap (asHtml . GJSON) $ items)
asHtmlQ (GJSON (JSON.Object items)) = (CQRaw, mconcat . map snd . List.sort . HashMap.toList . fmap (asHtml . GJSON) $ items)
asHtmlQ (GJSON (JSON.String txt)) = asHtmlQ (GText txt)
asHtmlQ (GJSON (JSON.Bool b)) = asHtmlQ (GBool b)
asHtmlQ (GJSON (JSON.Number n)) = asHtmlQ (GNumber n)
asHtmlQ (GJSON JSON.Null) = asHtmlQ GNull
asHtmlQ (GText x) = (CQRichDerived, toHtml x)
asHtmlQ (GHtml x) = (CQRichExplicit, x)
asHtmlQ (GBool x) = (CQPathological, toHtml $ if x then "1" :: Text else "")
asHtmlQ (GNumber s) = (CQRaw, toHtml . asText $ GNumber s)
asHtmlQ (GBytes s) = (CQRaw, toHtml . decodeUtf8 . Base64.encode $ s)
asHtmlQ (GMultiverse xs) =
  fromMaybe (CQPathological, mempty) . listToMaybe . reverse . List.sort . map asHtmlQ $ xs

asText :: GVal m -> Text
asText (GList xs) = mconcat . map asText $ xs
asText g@(GDict {}) = fromMaybe "" $ mconcat . map asText <$> asList g
asText x = snd . asTextQ $ x

asTextQ :: GVal m -> (ConversionQuality, Text)
asTextQ GNull = (CQPathological, mempty)
asTextQ (GList xs) = (CQRaw, mconcat . map asText $ xs)
asTextQ g@(GDict {}) = (CQRaw, fromMaybe "" $ mconcat . map asText <$> asList g)
asTextQ (GJSON (JSON.Array items)) = (CQRaw, Vector.foldl' (<>) mempty . fmap (asText . GJSON) $ items)
asTextQ (GJSON (JSON.Object items)) = (CQRaw, mconcat . map snd . List.sort . HashMap.toList . fmap (asText . GJSON) $ items)
asTextQ (GJSON (JSON.String txt)) = asTextQ (GText txt)
asTextQ (GJSON (JSON.Bool b)) = asTextQ (GBool b)
asTextQ (GJSON (JSON.Number n)) = asTextQ (GNumber n)
asTextQ (GJSON JSON.Null) = asTextQ GNull
asTextQ (GText x) = (CQRichDerived, x)
asTextQ (GHtml x) = (CQPathological, htmlSource x)
asTextQ (GBool x) = (CQPathological, if x then "1" else "")
asTextQ (GNumber s) =
  (CQRaw, Text.pack $
    either (const $ show s) show $ (floatingOrInteger s :: Either Double Integer))
asTextQ (GBytes s) = (CQRaw, decodeUtf8 . Base64.encode $ s)
asTextQ (GMultiverse xs) =
  fromMaybe (CQPathological, mempty) . listToMaybe . reverse . List.sort . map asTextQ $ xs

asBoolean :: GVal m -> Bool
asBoolean GNull = False
asBoolean (GList []) = False
asBoolean (GDict _ d) = not (Map.null d)
asBoolean (GJSON JSON.Null) = False
asBoolean (GJSON (JSON.Array v)) = Vector.null v
asBoolean (GJSON (JSON.Object h)) = HashMap.null h
asBoolean (GJSON (JSON.Number 0)) = False
asBoolean (GJSON (JSON.String "")) = False
asBoolean (GJSON (JSON.Bool b)) = b
asBoolean (GText "") = False
asBoolean (GHtml h) = htmlSource h == ""
asBoolean (GNumber 0) = False
asBoolean (GBytes "") = False
asBoolean (GBool b) = b
asBoolean (GMultiverse xs) = List.all asBoolean xs
asBoolean _ = True

asNumber :: GVal m -> Maybe Scientific
asNumber (GNumber n) = Just n
asNumber (GText t) = readMay . Text.unpack $ t
asNumber (GBool b) = Just $ if b then 1 else 0
asNumber (GJSON j) = case j of
  JSON.Number n -> Just n
  JSON.String s -> asNumber (GText s)
  JSON.Bool b -> asNumber (GBool b)
  _ -> Nothing
asNumber (GMultiverse xs) =
  listToMaybe . mapMaybe asNumber $ xs
asNumber _ = Nothing

asFunction :: GVal m -> Maybe (Function m)
asFunction (GFunction f) = Just f
asFunction (GMultiverse xs) =
  listToMaybe . mapMaybe asFunction $ xs
asFunction _ = Nothing

asBytes :: GVal m -> Maybe ByteString
asBytes (GBytes b) = Just b
asBytes (GMultiverse xs) =
  listToMaybe . mapMaybe asBytes $ xs
asBytes _ = Nothing

isNull :: GVal m -> Bool
isNull GNull = True
isNull (GJSON JSON.Null) = True
isNull (GMultiverse xs) = all isNull xs
isNull _ = False

asJSON :: GVal m -> Maybe JSON.Value
asJSON (GJSON j) = Just j
asJSON (GMultiverse xs) = listToMaybe . mapMaybe asJSON $ xs
asJSON _ = Nothing

glength :: GVal m -> Maybe Int
glength (GList a) = Just $ List.length a
glength (GDict _ a) = Just $ Map.size a
glength (GJSON (JSON.Array xs)) = Just $ Vector.length xs
glength (GJSON (JSON.Object xs)) = Just $ HashMap.size xs
glength (GMultiverse xs) = listToMaybe . mapMaybe glength $ xs
glength _ = Nothing

gappend :: GVal m -> GVal m -> GVal m
gappend GNull a = a
gappend a GNull = a
gappend (GList a) (GList b) = GList (a ++ b)
gappend (GDict ak am) (GDict bk bm) = GDict (ak `List.union` bk) (am <> bm)
gappend x y =
  GMultiverse
    [ GHtml (asHtml x <> asHtml y)
    , GText (asText x <> asText y)
    ]

-- | Marshal a GVal between carrier monads.
-- This will lose 'asFunction' information, because functions cannot be
-- transferred to other carrier monads, but it will keep all other data
-- structures intact.
marshalGVal :: GVal m -> GVal n
marshalGVal (GFunction f) = GNull -- functions cannot be marshalled
marshalGVal (GList xs) = GList $ map marshalGVal xs
marshalGVal (GDict ks d) = GDict ks $ fmap marshalGVal d
marshalGVal GNull = GNull
marshalGVal (GNumber n) = GNumber n
marshalGVal (GText t) = GText t
marshalGVal (GHtml h) = GHtml h
marshalGVal (GBool b) = GBool b
marshalGVal (GJSON j) = GJSON j
marshalGVal (GBytes b) = GBytes b
marshalGVal (GMultiverse xs) = GMultiverse $ map marshalGVal xs


-- | Marshal a GVal between carrier monads.
-- Unlike 'marshalGVal', 'asFunction' information is retained by hoisting
-- them using the provided hoisting functions. For 'Run' monads, which is
-- what 'GVal' is typically used with, the 'hoistRun' function can be used
-- to construct suitable hoisting functions.
marshalGValEx :: (Functor m, Functor n)
              => (forall a. m a -> n a)
              -> (forall a. n a -> m a)
              -> GVal m
              -> GVal n
marshalGValEx hoist unhoist (GFunction f) = GFunction $ marshalFunction hoist unhoist f
marshalGValEx hoist unhoist (GList xs) = GList $ fmap (marshalGValEx hoist unhoist) xs
marshalGValEx hoist unhoist (GDict ks xs) = GDict ks $ fmap (marshalGValEx hoist unhoist) xs
marshalGValEx _ _ g = marshalGVal g

marshalFunction :: (Functor m, Functor n) => (forall a. m a -> n a) -> (forall a. n a -> m a) -> Function m -> Function n
-- [(Maybe Text, GVal m)] -> m (GVal m)
marshalFunction hoist unhoist f args =
    let args' = [ (name, marshalGValEx unhoist hoist value)
                | (name, value) <- args
                ]
    in marshalGValEx hoist unhoist <$> hoist (f args')

-- | The default 'GVal' is equivalent to NULL.
instance Default (GVal m) where
    def = GNull

-- | Conversion to JSON values attempts the following conversions, in order:
--
-- - check the 'isNull' property; if it is 'True', always return 'Null',
--   even if the GVal implements 'asJSON'
-- - 'asJSON'
-- - 'asList'
-- - 'asDictItems' (through 'asHashMap')
-- - 'asNumber'
-- - 'asText'
--
-- Note that the default conversions will never return booleans unless 'asJSON'
-- explicitly does this, because 'asText' will always return *something*.
instance JSON.ToJSON (GVal m) where
    toJSON = gToJSON

gToJSON :: GVal m -> JSON.Value
gToJSON = snd . gToJSONQ

gToJSONQ :: GVal m -> (ConversionQuality, JSON.Value)
gToJSONQ GNull = (CQRichDerived, JSON.Null)
gToJSONQ (GList xs) = (CQRichDerived, JSON.Array . Vector.fromList . map gToJSON $ xs)
gToJSONQ (GDict _ xs) = (CQRichDerived, JSON.toJSON xs)
gToJSONQ (GText t) = (CQRaw, JSON.String t)
gToJSONQ (GHtml h) = (CQPathological, JSON.String $ htmlSource h)
gToJSONQ (GNumber n) = (CQRaw, JSON.Number n)
gToJSONQ (GBool b) = (CQRaw, JSON.Bool b)
gToJSONQ (GJSON j) = (CQRichExplicit, j)
gToJSONQ (GMultiverse xs) =
  fromMaybe (CQPathological, JSON.Null) . listToMaybe . reverse . List.sort . map gToJSONQ $ xs
gToJSONQ _ = (CQPathological, JSON.Null)

-- | For convenience, 'Show' is implemented in a way that looks similar to
-- JavaScript / JSON
instance Show (GVal m) where
    show = gShow

gShow :: GVal m -> String
gShow GNull = "null"
gShow (GFunction f) = "<<function>>"
gShow (GList xs) =
  "[" <> (mconcat . List.intersperse ", " . Prelude.map show $ xs) <> "]"
gShow (GDict ks kv) =
  let items = [ show k <> ": " <> show v | k <- ks, Just v <- [Map.lookup k kv] ]
  in "{" <> (mconcat . List.intersperse ", " $ items) <> "}"
gShow x = LText.unpack . LText.decodeUtf8 . JSON.encode $ x

-- | Converting to HTML hooks into the ToHtml instance for 'Text' for most tags.
-- Tags that have no obvious textual representation render as empty HTML.
instance ToHtml (GVal m) where
    toHtml = asHtml

instance PrintfArg (GVal m) where
    formatArg x fmt =
        case fmtChar (vFmt 's' fmt) of
            's' -> formatString
                    (Text.unpack $ asText x)
                    (fmt { fmtChar = 's', fmtPrecision = Nothing })
            'c' -> formatString
                    (Text.unpack $ asText x)
                    fmt
            f -> if f `Prelude.elem` ['f', 'F', 'g', 'G', 'e', 'E']
                    then formatRealFloat (toRealFloat . fromMaybe 0 . asNumber $ x) fmt
                    else formatInteger (Prelude.round . fromMaybe 0 . asNumber $ x) fmt

-- * Representing functions as 'GVal's
--
-- | A function that can be called from within a template execution context.
type Function m = [(Maybe Text, GVal m)] -> m (GVal m)

-- | Match arguments passed to a function at runtime against a list of declared
-- argument names.
-- @matchFuncArgs argNames argsPassed@ returns @(matchedArgs, positionalArgs, namedArgs)@,
-- where @matchedArgs@ is a list of arguments matched against declared names
-- (by name or by position), @positionalArgs@ are the unused positional
-- (unnamed) arguments, and @namedArgs@ are the unused named arguments.
matchFuncArgs :: [Text] -> [(Maybe Text, GVal m)] -> (HashMap Text (GVal m), [GVal m], HashMap Text (GVal m))
matchFuncArgs names args =
    (matched, positional, named)
    where
        positionalRaw = [ v | (Nothing, v) <- args ]
        namedRaw = HashMap.fromList [ (n, v) | (Just n, v) <- args ]
        fromPositional = Prelude.zip names positionalRaw
        numPositional = Prelude.length fromPositional
        namesRemaining = Prelude.drop numPositional names
        positional = Prelude.drop numPositional positionalRaw
        fromNamed = mapMaybe lookupName namesRemaining
        lookupName n = do
            v <- HashMap.lookup n namedRaw
            return (n, v)
        matched = HashMap.fromList $ fromPositional ++ fromNamed
        named = HashMap.difference namedRaw (HashMap.fromList fromNamed)

-- * Marshalling from Haskell to 'GVal'
--
-- | Types that implement conversion to 'GVal'.
class ToGVal m a where
    toGVal :: a -> GVal m

-- | Trivial instance for 'GVal' itself.
instance ToGVal m (GVal m) where
    toGVal = id

instance ToGVal m () where
    toGVal = const def

-- | 'Nothing' becomes NULL, 'Just' unwraps.
instance ToGVal m v => ToGVal m (Maybe v) where
    toGVal Nothing = GNull
    toGVal (Just x) = toGVal x

-- | Haskell lists become list-like 'GVal's
instance ToGVal m v => ToGVal m [v] where
    toGVal xs = GList (map toGVal xs)

-- | 'HashMap' of 'Text' becomes a dictionary 'GVal'
instance ToGVal m v => ToGVal m (HashMap Text v) where
    toGVal h = GDict (HashMap.keys h) . fmap toGVal . Map.fromList . HashMap.toList $ h

-- | 'Map' of 'Text' becomes a dictionary 'GVal'
instance ToGVal m v => ToGVal m (Map Text v) where
    toGVal m = GDict (Map.keys m) . fmap toGVal $ m

instance ToGVal m Int where
    toGVal = GNumber . fromIntegral

instance ToGVal m Integer where
    toGVal = GNumber . fromIntegral

instance ToGVal m Scientific where
    toGVal = GNumber

instance ToGVal m Day where
    toGVal x =
        let dayDict = dayToDict x
            julian = toModifiedJulianDay x
            formatted = Text.pack $ formatTime defaultTimeLocale "%0Y-%m-%d" x
        in GMultiverse
            [ GText formatted
            , GDict (map fst dayDict) (Map.fromList dayDict)
            , GList (List.map snd dayDict)
            ]

dayToDict :: Day -> [(Text, GVal m)]
dayToDict x =
    let (year, month, day) = toGregorian x
    in [ "year" ~> year
       , "month" ~> month
       , "day" ~> day
       ]

instance ToGVal m TimeOfDay where
    toGVal x =
        let timeDict = timeToDict x
            formatted = Text.pack $ formatTime defaultTimeLocale "%H:%M:%S" x
        in GMultiverse
            [ GText formatted
            , GDict (map fst timeDict) (Map.fromList timeDict)
            , GList (List.map snd timeDict)
            ]

timeToDict :: TimeOfDay -> [(Text, GVal m)]
timeToDict (TimeOfDay hours minutes seconds) =
    [ "hours" ~> hours
    , "minutes" ~> minutes
    , "seconds" ~> picoToScientific seconds
    ]

instance ToGVal m LocalTime where
    toGVal x =
        let dtDict = localTimeToDict x
            formatted = Text.pack $ formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S" x
        in GMultiverse
            [ GText formatted
            , GDict ["date", "time"] (Map.fromList $ dtDict ++
                [ "date" ~> localDay x
                , "time" ~> localTimeOfDay x
                ]
              )
            , GList (List.map snd dtDict)
            ]

localTimeToDict :: LocalTime -> [(Text, GVal m)]
localTimeToDict x =
        let dayDict = dayToDict $ localDay x
            timeDict = timeToDict $ localTimeOfDay x
        in dayDict ++ timeDict

instance ToGVal m TimeZone where
    toGVal  = dict . timeZoneToDict

timeZoneToDict :: TimeZone -> [(Text, GVal m)]
timeZoneToDict (TimeZone minutes summerOnly name) =
    [ "minutes" ~> minutes
    , "summerOnly" ~> summerOnly
    , "name" ~> name
    ]

instance ToGVal m TimeLocale where
    toGVal t =
        let formattedExample =
                Text.pack . formatTime t "%c" $
                    LocalTime (fromGregorian 2000 1 1) (TimeOfDay 13 15 00)
            timeLocaleDict = timeLocaleToDict t
        in GMultiverse
            [ GText formattedExample
            , GDict (map fst timeLocaleDict) (Map.fromList timeLocaleDict)
            ]

timeLocaleToDict :: TimeLocale -> [(Text, GVal m)]
timeLocaleToDict t =
    [ "wDays" ~> List.map packPair (wDays t)
    , "months" ~> List.map packPair (months t)
    , "amPm" ~> packPair (amPm t)
    , "dateTimeFmt" ~> Text.pack (dateTimeFmt t)
    , "dateFmt" ~> Text.pack (dateFmt t)
    , "timeFmt" ~> Text.pack (timeFmt t)
    , "time12Fmt" ~> Text.pack (time12Fmt t)
    -- TODO
    -- , "knownTimeZones" ~> knownTimeZones t
    , "knownTimeZones" ~> ([] :: [Text])
    ]

-- TODO: ToGVal instance for ZonedTime
instance ToGVal m ZonedTime where
    toGVal x =
        let dtDict = zonedTimeToDict x
            formatted = Text.pack $ formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S%z" x
        in GMultiverse
            [ GText formatted
            , dict dtDict
            ]

zonedTimeToDict :: ZonedTime -> [(Text, GVal m)]
zonedTimeToDict t =
    ("tz", toGVal $ zonedTimeZone t):localTimeToDict (zonedTimeToLocalTime t)

instance (ToGVal m a, ToGVal m b) => ToGVal m (a, b) where
    toGVal (a, b) = toGVal ([ toGVal a, toGVal b ] :: [GVal m])

instance (ToGVal m a, ToGVal m b, ToGVal m c) => ToGVal m (a, b, c) where
    toGVal (a, b, c) = toGVal ([ toGVal a, toGVal b, toGVal c ] :: [GVal m])

instance (ToGVal m a, ToGVal m b, ToGVal m c, ToGVal m d) => ToGVal m (a, b, c, d) where
    toGVal (a, b, c, d) = toGVal ([ toGVal a, toGVal b, toGVal c, toGVal d ] :: [GVal m])

-- | Silly helper function, needed to bypass the default 'Show' instance of
-- 'Scientific' in order to make integral 'Scientific's look like integers.
scientificToText :: Scientific -> Text
scientificToText x =
    Text.pack $ case floatingOrInteger x of
        Left x -> show x
        Right x -> show x

-- | Booleans render as 1 or empty string, and otherwise behave as expected.
instance ToGVal m Bool where
    toGVal x = GBool x

-- | 'String' -> 'GVal' conversion uses the 'IsString' class; because 'String'
-- is an alias for '[Char]', there is also a 'ToGVal' instance for 'String',
-- but it marshals strings as lists of characters, i.e., calling 'toGVal' on
-- a string produces a list of characters on the 'GVal' side.
instance IsString (GVal m) where
    fromString x = GText (Text.pack x)

-- | Single characters are treated as length-1 'Text's.
instance ToGVal m Char where
    toGVal = toGVal . Text.singleton

instance ToGVal m Text where
    toGVal = GText

instance ToGVal m LText.Text where
    toGVal = GText . LText.toStrict

instance ToGVal m ByteString where
    toGVal = GBytes

instance ToGVal m LBS.ByteString where
    toGVal = GBytes . LBS.toStrict

-- | This instance is slightly wrong; the 'asBoolean', 'asNumber', and 'asText'
-- methods all treat the HTML source as plain text. We do this to avoid parsing
-- the HTML back into a 'Text' (and dealing with possible parser errors); the
-- reason this instance exists at all is that we still want to be able to pass
-- pre-rendered HTML around sometimes, and as long as we don't call any numeric
-- or string functions on it, everything is fine. When such HTML values
-- accidentally do get used as strings, the HTML source will bleed into the
-- visible text, but at least this will not introduce an XSS vulnerability.
--
-- It is therefore recommended to avoid passing 'Html' values into templates,
-- and also to avoid calling any string functions on 'Html' values inside
-- templates (e.g. capturing macro output and then passing it through a textual
-- filter).
instance ToGVal m Html where
    toGVal = GHtml

instance ToGVal m JSON.Value where
    toGVal j = GJSON j

-- | Turn a 'Function' into a 'GVal'
fromFunction :: Function m -> GVal m
fromFunction = GFunction

-- * Convenience API for constructing heterogenous dictionaries.
--
-- Example usage:
--
-- > context :: GVal m
-- > context = dict [ "number" ~> (15 :: Int), "name" ~> ("Joe" :: String) ]

-- | A key/value pair, used for constructing dictionary GVals using a
-- compact syntax.
type Pair m = (Text, GVal m)

-- | Construct a dictionary GVal from a list of pairs. Internally, this uses
-- a hashmap, so element order will not be preserved.
dict :: [Pair m] -> GVal m
dict pairs = GDict (map fst pairs) (Map.fromList pairs)

orderedDict :: [Pair m] -> GVal m
orderedDict = dict

-- | Construct a pair from a key and a value.
(~>) :: ToGVal m a => Text -> a -> Pair m
k ~> v = (k, toGVal v)
infixr 8 ~>

-- * Convenience API for constructing heterogenous lists

type Cons m = [GVal m]

-- | Alias for '(~:)'.
gcons :: ToGVal m a => a -> Cons m -> Cons m
gcons = (:) . toGVal

-- | This operator allows constructing heterogenous lists using cons-style
-- syntax, e.g.:
--
-- >>> asText $ list ("Found " ~: (6 :: Int) ~: " items" ~: [] :: [GVal IO])
-- "Found 6 items"
(~:) :: ToGVal m a => a -> Cons m -> Cons m
(~:) = gcons
infixr 5 ~:

-- | Construct a GVal from a list of GVals. This is equivalent to the 'toGVal'
-- implementation of @[GVal m]@, but typed more narrowly for clarity and
-- disambiguation.
list :: Cons m -> GVal m
list = toGVal

-- * Inspecting 'GVal's / Marshalling 'GVal' to Haskell

-- | Check if the given GVal is a list-like object
isList :: GVal m -> Bool
isList = isJust . asList

-- | Check if the given GVal is a dictionary-like object
isDict :: GVal m -> Bool
isDict = isJust . asDictItems

-- | Treat a 'GVal' as a flat list and look up a value by integer index.
-- If the value is not a List, or if the index exceeds the list length,
-- return 'Nothing'.
lookupIndex :: Int -> GVal m -> Maybe (GVal m)
lookupIndex = lookupIndexMay . Just

-- | Helper function; look up a value by an integer index when the index may or
-- may not be available. If no index is given, return 'Nothing'.
lookupIndexMay :: Maybe Int -> GVal m -> Maybe (GVal m)
lookupIndexMay i v = do
    index <- i
    items <- asList v
    atMay items index

-- | Strictly-typed lookup: treat value as a dictionary-like object and look
-- up the value at a given key.
lookupKey :: Text -> GVal m -> Maybe (GVal m)
lookupKey k v = do
    lf <- asMap v
    Map.lookup k lf

-- | Loosely-typed lookup: try dictionary-style lookup first (treat index as
-- a string, and container as a dictionary), if that doesn't yield anything
-- (either because the index is not string-ish, or because the container
-- doesn't provide dictionary-style access), try index-based lookup.
lookupLoose :: GVal m -> GVal m -> Maybe (GVal m)
lookupLoose k v =
    lookupKey (asText k) v <|> lookupIndexMay (floor <$> asNumber k) v

-- | Like 'lookupLoose', but fall back to the given default value if
-- the key is not in the dictionary, or if the indexee is not a
-- dictionary-like object.
lookupLooseDef :: GVal m -> GVal m -> GVal m -> GVal m
lookupLooseDef d k = fromMaybe d . lookupLoose k

(~!) :: (FromGVal m v) => GVal m -> GVal m -> Maybe v
g ~! k = lookupLoose k g >>= fromGVal

-- | Treat a 'GVal' as a dictionary and list all the keys, with no particular
-- ordering.
keys :: GVal m -> Maybe [Text]
keys v = Prelude.map fst <$> asDictItems v

-- | Convert a 'GVal' to a number.
toNumber :: GVal m -> Maybe Scientific
toNumber = asNumber

-- | Convert a 'GVal' to an 'Int'.
-- The conversion will fail when the value is not numeric, and also if
-- it is too large to fit in an 'Int'.
toInt :: GVal m -> Maybe Int
toInt = toBoundedInteger <=< toNumber

-- | Convert a 'GVal' to an 'Integer'
-- The conversion will fail when the value is not an integer
toInteger :: GVal m -> Maybe Integer
toInteger = Prelude.either (const Nothing) Just . floatingOrInteger <=< asNumber

-- | Convert a 'GVal' to an 'Int', falling back to the given
-- default if the conversion fails.
toIntDef :: Int -> GVal m -> Int
toIntDef d = fromMaybe d . toInt

-- | Convert a 'GVal' to an 'Int', falling back to zero (0)
-- if the conversion fails.
toInt0 :: GVal m -> Int
toInt0 = toIntDef 0

-- | Loose cast to boolean.
--
-- Numeric zero, empty strings, empty lists, empty objects, 'Null', and boolean
-- 'False' are considered falsy, anything else (including functions) is
-- considered true-ish.
toBoolean :: GVal m -> Bool
toBoolean = asBoolean

-- | Dynamically cast to a function.
-- This yields 'Just' a 'Function' if the value is a function, 'Nothing' if
-- it's not.
toFunction :: GVal m -> Maybe (Function m)
toFunction = asFunction

picoToScientific :: Pico -> Scientific
picoToScientific (MkFixed x) = scientific x (-12)

scientificToPico :: Scientific -> Pico
scientificToPico s =
    MkFixed (Prelude.floor $ scientific (coefficient s) (base10Exponent s + 12))

{-#RULES "GVal/round-trip-Maybe" fromGVal . toGVal = Just #-}
{-#RULES "GVal/round-trip-Either" fromGValEither . toGVal = Right #-}
{-#RULES "GVal/text-shortcut" asText . toGVal = id #-}

class FromGVal m a where
    fromGValEither :: GVal m -> Either Prelude.String a
    fromGValEither = Prelude.maybe (Left "Conversion from GVal failed") Right . fromGVal
    fromGVal :: GVal m -> Maybe a
    fromGVal = Prelude.either (const Nothing) Just . fromGValEither

fromGValM :: (MonadFail m, FromGVal m a) => GVal m -> m a
fromGValM = Prelude.either Prelude.fail return . fromGValEither

instance FromGVal m Int where
    fromGVal = toInt

instance FromGVal m Scientific where
    fromGVal = asNumber

instance FromGVal m Integer where
    fromGVal = toInteger

instance FromGVal m Text where
    fromGVal = Just . asText

instance FromGVal m (GVal m) where
    fromGVal = Just

instance FromGVal m ByteString where
    fromGVal = asBytes

instance FromGVal m LBS.ByteString where
    fromGVal = fmap LBS.fromStrict . asBytes

instance FromGVal m a => FromGVal m (Maybe a) where
    fromGVal = \g ->
        if isNull g
            then
                Just Nothing
            else
                Just <$> fromGVal g

instance FromGVal m Bool where
    fromGVal = Just . asBoolean

instance FromGVal m JSON.Value where
    fromGVal = asJSON

instance FromGVal m () where
    fromGVal g = if isNull g then Just () else Nothing

instance FromGVal m a => FromGVal m [a] where
    fromGVal g = asList g >>= mapM fromGVal

instance ( FromGVal m a
         , FromGVal m b
         ) => FromGVal m (a, b) where
    fromGVal g = case asList g of
        Just [a, b] ->
            (,) <$> fromGVal a
                <*> fromGVal b
        _ -> Nothing

instance ( FromGVal m a
         , FromGVal m b
         , FromGVal m c
         ) => FromGVal m (a, b, c) where
    fromGVal g = case asList g of
        Just [a, b, c] ->
            (,,) <$> fromGVal a
                 <*> fromGVal b
                 <*> fromGVal c
        _ -> Nothing

instance ( FromGVal m a
         , FromGVal m b
         , FromGVal m c
         , FromGVal m d
         ) => FromGVal m (a, b, c, d) where
    fromGVal g = case asList g of
        Just [a, b, c, d] ->
            (,,,) <$> fromGVal a
                  <*> fromGVal b
                  <*> fromGVal c
                  <*> fromGVal d
        _ -> Nothing

instance FromGVal m Day where
    fromGVal g = do
        year <- fromIntegral <$> (g ~! "year" :: Maybe Int)
        month <- g ~! "month"
        day <- g ~! "day"
        return $ fromGregorian year month day

instance FromGVal m TimeOfDay where
    fromGVal g = do
        hours <- g ~! "hours"
        minutes <- g ~! "minutes"
        seconds <- scientificToPico <$> g ~! "seconds"
        return $ TimeOfDay hours minutes seconds

instance FromGVal m LocalTime where
    fromGVal g = do
        date <- fromGVal g <|> g ~! "date"
        time <- fromGVal g <|> g ~! "time"
        return $ LocalTime date time

instance FromGVal m ZonedTime where
    fromGVal g = do
        localTime <- fromGVal g
        timeZone <- g ~! "tz"
        return $ ZonedTime localTime timeZone

instance FromGVal m TimeZone where
    fromGVal g =
        TimeZone
            <$> g ~! "minutes"
            <*> g ~! "summerOnly"
            <*> (Text.unpack <$> g ~! "name")

instance FromGVal m TimeLocale where
    fromGVal g =
        if isDict g
            then
                Just $ TimeLocale
                    (fromMaybe (wDays defaultTimeLocale) $ List.map unpackPair <$> g ~! "wDays")
                    (fromMaybe (months defaultTimeLocale) $ List.map unpackPair <$> g ~! "months")
                    (fromMaybe (amPm defaultTimeLocale) $ unpackPair <$> g ~! "amPm")
                    (fromMaybe (dateTimeFmt defaultTimeLocale) $ Text.unpack <$> g ~! "dateTimeFmt")
                    (fromMaybe (dateFmt defaultTimeLocale) $ Text.unpack <$> g ~! "dateFmt")
                    (fromMaybe (timeFmt defaultTimeLocale) $ Text.unpack <$> g ~! "timeFmt")
                    (fromMaybe (time12Fmt defaultTimeLocale) $ Text.unpack <$> g ~! "time12Fmt")
                    (fromMaybe (knownTimeZones defaultTimeLocale) $ g ~! "knownTimeZones")
            else
                Nothing

pairwise :: (a -> b) -> (a, a) -> (b, b)
pairwise f (a, b) = (f a, f b)

packPair :: ([Char], [Char]) -> (Text, Text)
packPair = pairwise Text.pack

unpackPair :: (Text, Text) -> ([Char], [Char])
unpackPair = pairwise Text.unpack
