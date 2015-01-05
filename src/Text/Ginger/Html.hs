{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleInstances #-}
module Text.Ginger.Html
( Html
, preEscaped
, html
, htmlSource
, ToHtml (..)
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid

newtype Html = Html { unHtml :: Text }
    deriving (Monoid, Show, Eq, Ord)

class ToHtml s where
    toHtml :: s -> Html

instance ToHtml Text where
    toHtml = html

instance ToHtml [Char] where
    toHtml = mconcat . map htmlEncodeChar

htmlSource :: Html -> Text
htmlSource = unHtml

preEscaped :: Text -> Html
preEscaped = Html

html :: Text -> Html
html = mconcat . map htmlEncodeChar . Text.unpack

htmlEncodeChar :: Char -> Html
htmlEncodeChar '&' = Html "&amp;"
htmlEncodeChar '"' = Html "&quot;"
htmlEncodeChar '\'' = Html "&apos;"
htmlEncodeChar '<' = Html "&lt;"
htmlEncodeChar '>' = Html "&gt;"
htmlEncodeChar c = Html $ Text.singleton c
