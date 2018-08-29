-- | A HTML type, useful for implementing type-safe conversion between plain
-- text and HTML.
-- The HTML representation used here assumed Unicode throughout, and UTF-8
-- should be used as the encoding when sending @Html@ objects as responses to a
-- HTTP client.
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleInstances #-}
module Text.Ginger.Html
( Html
, unsafeRawHtml
, html
, htmlSource
, ToHtml (..)
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Data.Semigroup

-- | A chunk of HTML source.
newtype Html = Html { unHtml :: Text }
    deriving (Semigroup, Monoid, Show, Eq, Ord)

-- | Types that support conversion to HTML.
class ToHtml s where
    toHtml :: s -> Html

-- | Text is automatically HTML-encoded
instance ToHtml Text where
    toHtml = html

-- | String is automatically HTML-encoded and converted to 'Text'
instance ToHtml [Char] where
    toHtml = mconcat . map htmlEncodeChar

-- | Html itself is a trivial instance
instance ToHtml Html where
    toHtml = id

-- | Extract HTML source code from an @Html@ value.
htmlSource :: Html -> Text
htmlSource = unHtml

-- | Convert a chunk of HTML source code into an @Html@ value as-is. Note that
-- this bypasses any and all HTML encoding; the caller is responsible for
-- taking appropriate measures against XSS and other potential vulnerabilities.
-- In other words, the input to this function is considered pre-sanitized.
unsafeRawHtml :: Text -> Html
unsafeRawHtml = Html

-- | Safely convert plain text to HTML.
html :: Text -> Html
html = mconcat . map htmlEncodeChar . Text.unpack

-- | HTML-encode an individual character.
htmlEncodeChar :: Char -> Html
htmlEncodeChar '&' = Html "&amp;"
htmlEncodeChar '"' = Html "&quot;"
htmlEncodeChar '\'' = Html "&apos;"
htmlEncodeChar '<' = Html "&lt;"
htmlEncodeChar '>' = Html "&gt;"
htmlEncodeChar c = Html $ Text.singleton c
