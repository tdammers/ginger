{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
-- | GVal is a generic unitype value, parametrized over a user type.
-- It implements 'GingerValue' semantics such that wrapped 'UserValue's are
-- preserved as much as possible, promoting to specific other constructors
-- when needed.
module Text.Ginger.GVal
where

import Prelude ( (.), ($), (==), (/=)
               , (+), (-), (*), (/), div
               , undefined, otherwise, id
               , Maybe (..)
               , Bool (..)
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
import Safe (readMay)
import Data.Monoid
import Data.Scientific (Scientific)
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector

import Text.Ginger.Html

type Function m = ([(Maybe Text, GVal m)] -> m (GVal m))

data GVal m =
    List [GVal m] |
    Object (HashMap Text (GVal m)) |
    String Text |
    Html Html |
    Boolean Bool |
    Number Scientific |
    Null |
    Function (Function m)

class ToGVal m a where
    toGVal :: a -> GVal m

instance ToGVal m (GVal m) where
    toGVal = id

instance Show (GVal m) where
    show (List xs) = "[" <> (mconcat . List.intersperse ", " . Prelude.map show $ xs) <> "]"
    show (String v) = show v
    show (Html h) = show h
    show (Boolean b) = show b
    show (Number n) = show n
    show Null = "null"
    show (Function _) = "<<function>>"

instance ToHtml (GVal m) where
    toHtml (List xs) = mconcat . Prelude.map toHtml $ xs
    toHtml (String s) = toHtml s
    toHtml (Html h) = h
    toHtml (Number n) = toHtml . Text.pack . show $ n
    toHtml (Boolean False) = html ""
    toHtml (Boolean True) = html "1"
    toHtml _ = html ""

lookup :: Text -> GVal m -> Maybe (GVal m)
lookup k _ = Nothing

keys :: GVal m -> [GVal m]
keys _ = []

toList :: GVal m -> [GVal m]
toList (List xs) = xs
toList _ = []

toNumber :: GVal m -> Maybe Scientific
toNumber (Number n) = Just n
toNumber (String s) = readMay . Text.unpack $ s
toNumber (Boolean False) = Nothing
toNumber (Boolean True) = Just 1
toNumber _ = Nothing

toBoolean :: GVal m -> Bool
toBoolean (Number n) = n /= 0
toBoolean (String s) = not $ Text.null s
toBoolean (List xs) = not $ List.null xs
toBoolean (Boolean b) = b
toBoolean (Function _) = True
toBoolean _ = False

toFunction :: GVal m -> Maybe (Function m)
toFunction (Function f) = Just f
toFunction _ = Nothing

instance ToGVal m JSON.Value where
    toGVal (JSON.Number n) = Number n
    toGVal (JSON.String s) = String s
    toGVal (JSON.Bool b) = Boolean b
    toGVal (JSON.Null) = Null
    toGVal (JSON.Array a) = List (List.map toGVal $ Vector.toList a)
    toGVal (JSON.Object o) = Object (HashMap.map toGVal $ o)
