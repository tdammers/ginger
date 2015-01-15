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
               )
import qualified Prelude
import qualified Data.List as List
import Data.Maybe ( fromMaybe, catMaybes, isJust )
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
import Control.Monad ( forM, mapM )
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
        { asList :: [GVal m]
        , asDictItems :: [(Text, GVal m)]
        , asLookup :: Text -> Maybe (GVal m)
        , asHtml :: Html
        , asText :: Text
        , asBoolean :: Bool
        , asNumber :: Maybe Scientific
        , asFunction :: Maybe (Function m)
        , isNull :: Bool
        , isFunction :: Bool
        , isList :: Bool
        , isDict :: Bool
        , length :: Int
        }

instance Default (GVal m) where
    def = GVal
            { asList = []
            , asDictItems = []
            , asLookup = const def
            , asHtml = unsafeRawHtml ""
            , asText = ""
            , asBoolean = False
            , asNumber = Nothing
            , asFunction = Nothing
            , isNull = True
            , isFunction = False
            , isList = False
            , isDict = False
            , length = 0
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
        | isFunction v = "<<function>>"
        | isDict v = "{" <> (mconcat . List.intersperse ", " $ [ show k <> ": " <> show v | (k, v) <- asDictItems v ]) <> "}"
        | isList v = "[" <> (mconcat . List.intersperse ", " . Prelude.map show $ asList v) <> "]"
        | isJust (asNumber v) =
            case floatingOrInteger <$> asNumber v :: Maybe (Either Double Integer) of
                Just (Left x) -> show (asNumber v)
                Just (Right x) -> show x
                Nothing -> ""
        | otherwise = Text.unpack $ asText v

-- | Converting to HTML hooks into the ToHtml instance for 'Text' for most tags.
-- Tags that have no obvious textual representation render as empty HTML.
instance ToHtml (GVal m) where
    toHtml = asHtml

-- | Treat a 'GVal' as a flat list and look up a value by index.
-- If the value is not a List, or if the index exceeds the list length,
-- return 'Nothing'.
lookupIndex :: Int -> GVal m -> Maybe (GVal m)
lookupIndex i v = atMay (asList v) i

lookupLoose :: GVal m -> GVal m -> Maybe (GVal m)
lookupLoose k v
    | isDict v = asLookup v (asText k)
    | isList v = lookupIndex (floor . fromMaybe 0 . asNumber $ k) v
    | otherwise = Nothing

-- | Treat a 'GVal' as a dictionary and list all the keys, with no particular
-- ordering.
keys :: GVal m -> [Text]
keys v = Prelude.map fst $ asDictItems v

-- | List the keys for list-like values. For dictionaries, these are the
-- dictionary keys, in no particular order; for plain lists, they are
-- 0-based integer indexes.
iterKeys :: GVal m -> [GVal m]
iterKeys v
    | isDict v = Prelude.map toGVal . keys $ v
    | isList v = Prelude.map toGVal [0..length v - 1]
    | otherwise = []

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

fromFunction :: Function m -> GVal m
fromFunction f =
    def
        { asHtml = html ""
        , asText = ""
        , asBoolean = True
        , isNull = False
        , isFunction = True
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
                    , isList = True
                    , asList = Prelude.map toGVal xs
                    , length = Prelude.length xs
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
                    , isDict = True
                    , asLookup = \v -> HashMap.lookup v xs
                    , asDictItems = HashMap.toList xs
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

instance ToGVal m Scientific where
    toGVal x =
        def
            { asHtml = html . Text.pack . show $ x
            , asText = Text.pack . show $ x
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

instance ToGVal m [Char] where
    toGVal x =
        def
            { asHtml = html . Text.pack $ x
            , asText = Text.pack x
            , asBoolean = not $ Prelude.null x
            , asNumber = readMay x
            , isNull = False
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
