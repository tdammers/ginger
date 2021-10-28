{-#LANGUAGE CPP #-}
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

import Prelude ( (.), ($), (==), (/=)
               , (++), (+), (-), (*), (/), div
               , (=<<), (>>=), return
               , (||), (&&)
               , undefined, otherwise, id, const
               , fmap
               , Maybe (..)
               , Bool (..)
               , Either (..)
               , Char
               , Int
               , Integer
               , Double
               , Show, show
               , Integral
               , fromIntegral, floor
               , not
               , fst, snd
               , Monad
               , Functor
               )
import Control.Monad.Fail (MonadFail)
import qualified Prelude
import Data.Maybe ( fromMaybe, catMaybes, isJust, mapMaybe )
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
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
#endif
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
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LText

import Text.Ginger.Html

-- * The Ginger Value type
--
-- | A variant type designed as the unitype for the template language. Any
-- value referenced in a template, returned from within a template, or used
-- in a template context, will be a 'GVal'.
-- @m@, in most cases, should be a 'Monad'.
--
-- Some laws apply here, most notably:
--
-- - when 'isNull' is 'True', then all of 'asFunction', 'asText', 'asNumber',
--   'asHtml', 'asList', 'asDictItems', and 'length' should produce 'Nothing'
-- - when 'isNull' is 'True', then 'asBoolean' should produce 'False'
-- - when 'asNumber' is not 'Nothing', then 'asBoolean' should only return
--   'False' for exactly zero
-- - 'Nothing'-ness of 'length' should match one or both of 'asList' / 'asDictItems'
data GVal m =
    GVal
        { asList :: Maybe [GVal m] -- ^ Convert value to list, if possible
        , asDictItems :: Maybe [(Text, GVal m)] -- ^ Convert value to association list ("dictionary"), if possible
        , asLookup :: Maybe (Text -> Maybe (GVal m)) -- ^ Convert value to a lookup function
        , asHtml :: Html -- ^ Render value as HTML
        , asText :: Text -- ^ Render value as plain-text
        , asBoolean :: Bool -- ^ Get value's truthiness
        , asNumber :: Maybe Scientific -- ^ Convert value to a number, if possible
        , asFunction :: Maybe (Function m) -- ^ Access value as a callable function, if it is one
        , asBytes :: Maybe ByteString -- ^ Access as raw bytes
        , length :: Maybe Int -- ^ Get length of value, if it is a collection (list/dict)
        , isNull :: Bool -- ^ Check if the value is null
        , asJSON :: Maybe JSON.Value -- ^ Provide a custom JSON representation of the value
        }

gappend :: GVal m -> GVal m -> GVal m
gappend a b =
  GVal
    { asList = (++) <$> asList a <*> asList b
    , asDictItems = (++) <$> asDictItems a <*> asDictItems b
    , asLookup = do
        lookupA <- asLookup a
        lookupB <- asLookup b
        return $ \k -> lookupA k <|> lookupB k
    , asHtml = asHtml a <> asHtml b
    , asText = asText a <> asText b
    , asBytes = asBytes a <> asBytes b
    , asBoolean = (asBoolean a || asBoolean b) && not (isNull a || isNull b)
    , asNumber = readMay . Text.unpack $ (asText a <> asText b)
    , asFunction = Nothing
    , isNull = isNull a || isNull b
    , asJSON = case (JSON.toJSON a, JSON.toJSON b) of
        (JSON.Array x, JSON.Array y) -> Just $ JSON.Array (x <> y)
        (JSON.Object x, JSON.Object y) -> Just $ JSON.Object (x <> y)
        (JSON.String x, JSON.String y) -> Just $ JSON.String (x <> y)
        (JSON.Null, b) -> Just $ b
        (a, JSON.Null) -> Just $ a
        _ -> Nothing -- If JSON tags mismatch, use default toJSON impl
    , length = (+) <$> length a <*> length b
    }

-- | Marshal a GVal between carrier monads.
-- This will lose 'asFunction' information, because functions cannot be
-- transferred to other carrier monads, but it will keep all other data
-- structures intact.
marshalGVal :: GVal m -> GVal n
marshalGVal g =
    GVal
        { asList = fmap marshalGVal <$> asList g
        , asDictItems = fmap (\items -> [(k, marshalGVal v) | (k, v) <- items]) (asDictItems g)
        , asLookup = fmap (fmap marshalGVal .) (asLookup g)
        , asHtml = asHtml g
        , asText = asText g
        , asBytes = asBytes g
        , asBoolean = asBoolean g
        , asNumber = asNumber g
        , asFunction = Nothing
        , isNull = isNull g
        , length = length g
        , asJSON = asJSON g
        }

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
marshalGValEx hoist unhoist g =
    GVal
        { asList = fmap (marshalGValEx hoist unhoist) <$> asList g
        , asDictItems = fmap (\items -> [(k, marshalGValEx hoist unhoist v) | (k, v) <- items]) (asDictItems g)
        , asLookup = fmap (fmap (marshalGValEx hoist unhoist) .) (asLookup g)
        , asHtml = asHtml g
        , asText = asText g
        , asBytes = asBytes g
        , asBoolean = asBoolean g
        , asNumber = asNumber g
        , asFunction = marshalFunction hoist unhoist <$> asFunction g
        , isNull = isNull g
        , length = length g
        , asJSON = asJSON g
        }

marshalFunction :: (Functor m, Functor n) => (forall a. m a -> n a) -> (forall a. n a -> m a) -> Function m -> Function n
-- [(Maybe Text, GVal m)] -> m (GVal m)
marshalFunction hoist unhoist f args =
    let args' = [ (name, marshalGValEx unhoist hoist value)
                | (name, value) <- args
                ]
    in marshalGValEx hoist unhoist <$> hoist (f args')

-- | Convenience wrapper around 'asDictItems' to represent a 'GVal' as a
-- 'HashMap'.
asHashMap :: GVal m -> Maybe (HashMap Text (GVal m))
asHashMap g = HashMap.fromList <$> asDictItems g

-- | The default 'GVal' is equivalent to NULL.
instance Default (GVal m) where
    def = GVal
            { asList = Nothing
            , asDictItems = Nothing
            , asLookup = Nothing
            , asHtml = unsafeRawHtml ""
            , asText = ""
            , asBytes = Nothing
            , asBoolean = False
            , asNumber = Nothing
            , asFunction = Nothing
            , isNull = True
            , length = Nothing
            , asJSON = Nothing
            }

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
    toJSON g =
        if isNull g
            then JSON.Null
            else fromMaybe (JSON.toJSON $ asText g) $
                    asJSON g <|>
                    (JSON.toJSON <$> asList g) <|>
                    (JSON.toJSON <$> asHashMap g) <|>
                    (JSON.toJSON <$> asNumber g)

-- | For convenience, 'Show' is implemented in a way that looks similar to
-- JavaScript / JSON
instance Show (GVal m) where
    show v
        | isNull v = "null"
        | isJust (asFunction v) = "<<function>>"
        | isJust (asDictItems v) =
            let items = [ show k <> ": " <> show v | (k, v) <- fromMaybe [] (asDictItems v) ]
                      ++ [ show k <> ": " <> show v | (k, v) <- Prelude.zip [0..] (fromMaybe [] $ asList v) ]
            in "{" <> (mconcat . List.intersperse ", " $ items) <> "}"
        | isJust (asList v) = "[" <> (mconcat . List.intersperse ", " . Prelude.map show $ fromMaybe [] (asList v)) <> "]"
        | isJust (asNumber v) =
            case floatingOrInteger <$> asNumber v :: Maybe (Either Double Integer) of
                Just (Left x) -> show (asNumber v)
                Just (Right x) -> show x
                Nothing -> ""
        | otherwise = show $ asText v

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
    toGVal Nothing = def { asJSON = Just JSON.Null }
    toGVal (Just x) = toGVal x

-- | Haskell lists become list-like 'GVal's
instance ToGVal m v => ToGVal m [v] where
    toGVal xs = helper (Prelude.map toGVal xs)
        where
            helper :: [GVal m] -> GVal m
            helper xs =
                def
                    { asHtml = mconcat . Prelude.map asHtml $ xs
                    , asText = mconcat . Prelude.map asText $ xs
                    , asBytes = mconcat . Prelude.map asBytes $ xs
                    , asBoolean = not . List.null $ xs
                    , isNull = False
                    , asList = Just $ Prelude.map toGVal xs
                    , length = Just $ Prelude.length xs
                    }

-- | 'HashMap' of 'Text' becomes a dictionary-like 'GVal'
instance ToGVal m v => ToGVal m (HashMap Text v) where
    toGVal xs = helper (HashMap.map toGVal xs)
        where
            helper :: HashMap Text (GVal m) -> GVal m
            helper xs =
                def
                    { asHtml = mconcat . Prelude.map asHtml . HashMap.elems $ xs
                    , asText = mconcat . Prelude.map asText . HashMap.elems $ xs
                    , asBytes = mconcat . Prelude.map asBytes . HashMap.elems $ xs
                    , asBoolean = not . HashMap.null $ xs
                    , isNull = False
                    , asLookup = Just (`HashMap.lookup` xs)
                    , asDictItems = Just $ HashMap.toList xs
                    }

-- | 'Map' of 'Text' becomes a dictionary-like 'GVal'
instance ToGVal m v => ToGVal m (Map Text v) where
    toGVal xs = helper (Map.map toGVal xs)
        where
            helper :: Map Text (GVal m) -> GVal m
            helper xs =
                def
                    { asHtml = mconcat . Prelude.map asHtml . Map.elems $ xs
                    , asText = mconcat . Prelude.map asText . Map.elems $ xs
                    , asBytes = mconcat . Prelude.map asBytes . Map.elems $ xs
                    , asBoolean = not . Map.null $ xs
                    , isNull = False
                    , asLookup = Just (`Map.lookup` xs)
                    , asDictItems = Just $ Map.toAscList xs
                    }

instance ToGVal m Int where
    toGVal x =
        def
            { asHtml = html . Text.pack . show $ x
            , asText = Text.pack . show $ x
            , asBoolean = x /= 0
            , asNumber = Just . fromIntegral $ x
            , isNull = False
            }

instance ToGVal m Integer where
    toGVal x =
        def
            { asHtml = html . Text.pack . show $ x
            , asText = Text.pack . show $ x
            , asBytes = Just . encodeUtf8 . Text.pack . show $ x
            , asBoolean = x /= 0
            , asNumber = Just . fromIntegral $ x
            , isNull = False
            }

instance ToGVal m Scientific where
    toGVal x =
        def
            { asHtml = html $ scientificToText x
            , asText = scientificToText x
            , asBytes = Just . encodeUtf8 . scientificToText $ x
            , asBoolean = x /= 0
            , asNumber = Just x
            , isNull = False
            }

instance ToGVal m Day where
    toGVal x =
        let dayDict = dayToDict x
            julian = toModifiedJulianDay x
            formatted = Text.pack $ formatTime defaultTimeLocale "%0Y-%m-%d" x
        in (orderedDict dayDict)
            { asHtml = html $ formatted
            , asText = formatted
            , asBytes = Just . encodeUtf8 $ formatted
            , asBoolean = True
            , asNumber = Just . fromIntegral $ julian
            , asList = Just (List.map snd dayDict)
            }

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
        in (orderedDict timeDict)
            { asHtml = html $ formatted
            , asText = formatted
            , asBytes = Just . encodeUtf8 $ formatted
            , asBoolean = True
            , asNumber = Nothing
            , asList = Just (List.map snd timeDict)
            }

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
        in (orderedDict $
                dtDict ++
                [ "date" ~> localDay x
                , "time" ~> localTimeOfDay x
                ])
            { asHtml = html $ formatted
            , asText = formatted
            , asBytes = Just . encodeUtf8 $ formatted
            , asBoolean = True
            , asNumber = Nothing
            , asList = Just (List.map snd dtDict)
            }

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
        in (dict timeLocaleDict)
            { asHtml = html $ formattedExample
            , asText = formattedExample
            , asBytes = Just . encodeUtf8 $ formattedExample
            , asBoolean = True
            , asNumber = Nothing
            }

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
        in (dict dtDict)
            { asHtml = html $ formatted
            , asText = formatted
            , asBytes = Just . encodeUtf8 $ formatted
            , asBoolean = True
            , asNumber = Nothing
            }

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
    toGVal x =
        def
            { asHtml = if x then html "1" else html ""
            , asText = if x then "1" else ""
            , asBoolean = x
            , asBytes = Just $ if x then "1" else "0"
            , asNumber = Just $ if x then 1 else 0
            , isNull = False
            , asJSON = Just (JSON.Bool x)
            }

-- | 'String' -> 'GVal' conversion uses the 'IsString' class; because 'String'
-- is an alias for '[Char]', there is also a 'ToGVal' instance for 'String',
-- but it marshals strings as lists of characters, i.e., calling 'toGVal' on
-- a string produces a list of characters on the 'GVal' side.
instance IsString (GVal m) where
    fromString x =
        def
            { asHtml = html . Text.pack $ x
            , asText = Text.pack x
            , asBytes = Just . encodeUtf8 . Text.pack $ x
            , asBoolean = not $ Prelude.null x
            , asNumber = readMay x
            , isNull = False
            , length = Just . Prelude.length $ x
            }

-- | Single characters are treated as length-1 'Text's.
instance ToGVal m Char where
    toGVal = toGVal . Text.singleton

instance ToGVal m Text where
    toGVal x =
        def
            { asHtml = html x
            , asText = x
            , asBytes = Just . encodeUtf8 $ x
            , asBoolean = not $ Text.null x
            , asNumber = readMay . Text.unpack $ x
            , isNull = False
            }

instance ToGVal m LText.Text where
    toGVal x =
        def
            { asHtml = html (LText.toStrict x)
            , asText = LText.toStrict x
            , asBytes = Just . LBS.toStrict . LText.encodeUtf8 $ x
            , asBoolean = not $ LText.null x
            , asNumber = readMay . LText.unpack $ x
            , isNull = False
            }

instance ToGVal m ByteString where
    toGVal x =
        def
            { asHtml = html (decodeUtf8 x)
            , asText = decodeUtf8 x
            , asBytes = Just x
            , asBoolean = not $ BS.null x
            , asNumber = readMay . Text.unpack . decodeUtf8 $ x
            , isNull = False
            }

instance ToGVal m LBS.ByteString where
    toGVal x =
        def
            { asHtml = html . LText.toStrict . LText.decodeUtf8 $ x
            , asText = LText.toStrict . LText.decodeUtf8 $ x
            , asBytes = Just . LBS.toStrict $ x
            , asBoolean = not $ LBS.null x
            , asNumber = readMay . LText.unpack . LText.decodeUtf8 $ x
            , isNull = False
            }
--
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
    toGVal x =
        def
            { asHtml = x
            , asText = htmlSource x
            , asBoolean = not . Text.null . htmlSource $ x
            , asNumber = readMay . Text.unpack . htmlSource $ x
            , isNull = False
            }

-- | Convert Aeson 'Value's to 'GVal's over an arbitrary host monad. Because
-- JSON cannot represent functions, this conversion will never produce a
-- 'Function'. Further, the 'ToJSON' instance for such a 'GVal' will always
-- produce the exact 'Value' that was use to construct the it.
instance ToGVal m JSON.Value where
    toGVal j = (rawJSONToGVal j) { asJSON = Just j }

rawJSONToGVal :: JSON.Value -> GVal m
rawJSONToGVal (JSON.Number n) = toGVal n
rawJSONToGVal (JSON.String s) = toGVal s
rawJSONToGVal (JSON.Bool b) = toGVal b
rawJSONToGVal JSON.Null = def
rawJSONToGVal (JSON.Array a) = toGVal $ Vector.toList a
rawJSONToGVal (JSON.Object o) = toGVal o

#if MIN_VERSION_aeson(2,0,0)
-- | 'AKM.KeyMap' of 'JSON.Value' becomes a dictionary-like 'GVal'
instance ToGVal m (AKM.KeyMap JSON.Value) where
    toGVal xs = helper (AKM.map toGVal xs)
        where
            helper :: AKM.KeyMap (GVal m) -> GVal m
            helper xs =
                def
                    { asHtml = mconcat . List.map (asHtml . snd) . AKM.toList $ xs
                    , asText = mconcat . List.map (asText . snd) . AKM.toList $ xs
                    , asBytes = mconcat . List.map (asBytes . snd) . AKM.toList $ xs
                    , asBoolean = not . AKM.null $ xs
                    , isNull = False
                    , asLookup = Just $ (`AKM.lookup` xs) . AK.fromText
                    , asDictItems = Just . List.map (\(k,v) -> (AK.toText k, v)) . AKM.toList $ xs
                    }
#endif

-- | Turn a 'Function' into a 'GVal'
fromFunction :: Function m -> GVal m
fromFunction f =
    def
        { asHtml = html ""
        , asText = ""
        , asBoolean = True
        , isNull = False
        , asFunction = Just f
        , asJSON = Just "<<function>>"
        }


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
dict = toGVal . HashMap.fromList

-- | Construct an ordered dictionary GVal from a list of pairs. Internally,
-- this conversion uses both a hashmap (for O(1) lookup) and the original list,
-- so element order is preserved, but there is a bit of a memory overhead.
orderedDict :: [Pair m] -> GVal m
orderedDict xs =
    def
        { asHtml = mconcat . Prelude.map (asHtml . snd) $ xs
        , asText = mconcat . Prelude.map (asText . snd) $ xs
        , asBoolean = not . Prelude.null $ xs
        , isNull = False
        , asLookup = Just (`HashMap.lookup` hm)
        , asDictItems = Just xs
        }
    where
        hm = HashMap.fromList xs

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
    lf <- asLookup v
    lf k

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
