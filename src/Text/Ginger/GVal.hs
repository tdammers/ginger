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

-- | Ginger value.
data GVal m =
    GVal
        { asList :: Maybe [GVal m]
        , asDictItems :: Maybe [(Text, GVal m)]
        , asLookup :: Maybe (Text -> Maybe (GVal m))
        , asHtml :: Html
        , asText :: Text
        , asBoolean :: Bool
        , asNumber :: Maybe Scientific
        , asFunction :: Maybe (Function m)
        , length :: Maybe Int
        , isNull :: Bool
        }

isList :: GVal m -> Bool
isList = isJust . asList

isDict :: GVal m -> Bool
isDict = isJust . asDictItems

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

-- | Types that implement conversion to 'GVal'
class ToGVal m a where
    toGVal :: a -> GVal m

-- | Trivial instance for 'GVal' itself
instance ToGVal m (GVal m) where
    toGVal = id

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

instance ToGVal m v => ToGVal m (Maybe v) where
    toGVal Nothing = def
    toGVal (Just x) = toGVal x

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
            { asHtml = html . Text.pack $
                        fromMaybe (show x)
                        (show <$> (toBoundedInteger x :: Maybe Int))
            , asText = Text.pack $
                        fromMaybe (show x)
                        (show <$> (toBoundedInteger x :: Maybe Int))
            , asBoolean = x /= 0
            , asNumber = Just x
            , isNull = False
            }

instance ToGVal m Bool where
    toGVal x =
        def
            { asHtml = if x then html "1" else html ""
            , asText = if x then "1" else ""
            , asBoolean = x
            , asNumber = Just $ if x then 1 else 0
            , isNull = False
            }

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

instance ToGVal m Text where
    toGVal x =
        def
            { asHtml = html x
            , asText = x
            , asBoolean = not $ Text.null x
            , asNumber = readMay . Text.unpack $ x
            , isNull = False
            }

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
