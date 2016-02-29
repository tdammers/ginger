{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE ScopedTypeVariables #-}

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
               , (>>=), return
               , undefined, otherwise, id, const
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
               )
import qualified Prelude
import qualified Data.List as List
import Data.Maybe ( fromMaybe, catMaybes, isJust )
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
                       )
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import Control.Monad ( forM, mapM )
import Control.Monad.Trans (MonadTrans, lift)
import Data.Default (Default, def)

import Text.Ginger.Html

-- * The Ginger Value type
--
-- | A variant type designed as the unitype for the template language. Any
-- value referenced in a template, returned from within a template, or used
-- in a template context, will be a 'GVal'.
-- @m@, in most cases, should be a 'Monad'.
--
-- | Some laws apply here, most notably:
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
        , length :: Maybe Int -- ^ Get length of value, if it is a collection (list/dict)
        , isNull :: Bool -- ^ Check if the value is null
        }

-- | The default 'GVal' is equivalent to NULL.
instance Default (GVal m) where
    def = GVal
            { asList = Nothing
            , asDictItems = Nothing
            , asLookup = Nothing
            , asHtml = unsafeRawHtml ""
            , asText = ""
            , asBoolean = False
            , asNumber = Nothing
            , asFunction = Nothing
            , isNull = True
            , length = Nothing
            }

-- | For convenience, 'Show' is implemented in a way that looks similar to
-- JavaScript / JSON
instance Show (GVal m) where
    show v
        | isNull v = "null"
        | isJust (asFunction v) = "<<function>>"
        | isJust (asDictItems v) = "{" <> (mconcat . List.intersperse ", " $ [ show k <> ": " <> show v | (k, v) <- fromMaybe [] (asDictItems v) ]) <> "}"
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
        fromNamed = catMaybes $ (List.map lookupName namesRemaining)
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

-- | 'Nothing' becomes NULL, 'Just' unwraps.
instance ToGVal m v => ToGVal m (Maybe v) where
    toGVal Nothing = def
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
                    , asBoolean = not . HashMap.null $ xs
                    , isNull = False
                    , asLookup = Just (\v -> HashMap.lookup v xs)
                    , asDictItems = Just $ HashMap.toList xs
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
            , asBoolean = x /= 0
            , asNumber = Just . fromIntegral $ x
            , isNull = False
            }

instance ToGVal m Scientific where
    toGVal x =
        def
            { asHtml = html $ scientificToText x
            , asText = scientificToText x
            , asBoolean = x /= 0
            , asNumber = Just x
            , isNull = False
            }

instance (ToGVal m a, ToGVal m b) => ToGVal m (a, b) where
    toGVal (a, b) = toGVal ([ toGVal a, toGVal b ] :: [GVal m])

instance (ToGVal m a, ToGVal m b, ToGVal m c) => ToGVal m (a, b, c) where
    toGVal (a, b, c) = toGVal ([ toGVal a, toGVal b, toGVal c ] :: [GVal m])

instance (ToGVal m a, ToGVal m b, ToGVal m c, ToGVal m d) => ToGVal m (a, b, c, d) where
    toGVal (a, b, c, d) = toGVal ([ toGVal a, toGVal b, toGVal c, toGVal d ] :: [GVal m])

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
        { asHtml = mconcat . Prelude.map asHtml . Prelude.map snd $ xs
        , asText = mconcat . Prelude.map asText . Prelude.map snd $ xs
        , asBoolean = not . Prelude.null $ xs
        , isNull = False
        , asLookup = Just (\v -> HashMap.lookup v hm)
        , asDictItems = Just xs
        }
    where
        hm = HashMap.fromList xs

-- | Construct a pair from a key and a value.
(~>) :: ToGVal m a => Text -> a -> Pair m
k ~> v = (k, toGVal v)

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
            , asNumber = Just $ if x then 1 else 0
            , isNull = False
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
            , asBoolean = not $ Text.null x
            , asNumber = readMay . Text.unpack $ x
            , isNull = False
            }

instance ToGVal m LText.Text where
    toGVal x =
        def
            { asHtml = html (LText.toStrict x)
            , asText = LText.toStrict x
            , asBoolean = not $ LText.null x
            , asNumber = readMay . LText.unpack $ x
            , isNull = False
            }

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
-- 'Function'.
instance ToGVal m JSON.Value where
    toGVal (JSON.Number n) = toGVal n
    toGVal (JSON.String s) = toGVal s
    toGVal (JSON.Bool b) = toGVal b
    toGVal (JSON.Null) = def
    toGVal (JSON.Array a) = toGVal $ Vector.toList a
    toGVal (JSON.Object o) = toGVal o

-- | Turn a 'Function' into a 'GVal'
fromFunction :: Function m -> GVal m
fromFunction f =
    def
        { asHtml = html ""
        , asText = ""
        , asBoolean = True
        , isNull = False
        , asFunction = Just f
        }


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
toInt x = toNumber x >>= toBoundedInteger

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
