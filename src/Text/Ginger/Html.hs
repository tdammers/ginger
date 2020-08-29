-- | A HTML type, useful for implementing type-safe conversion between plain
-- text and HTML.
-- The HTML representation used here assumed Unicode throughout, and UTF-8
-- should be used as the encoding when sending @Html@ objects as responses to a
-- HTTP client.
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeFamilies #-}
module Text.Ginger.Html
( Html
, unsafeRawHtml
, html
, htmlSource
, ToHtml (..)
, HtmlBuilder
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText
import Data.Semigroup as Semigroup
import Text.Ginger.Buildable

-- | A chunk of HTML source.
newtype Html = Html { unHtml :: Text }
    deriving (Semigroup.Semigroup, Monoid, Show, Eq, Ord)

newtype HtmlBuilder = HtmlBuilder { unHtmlBuilder :: LText.Builder }
    deriving (Semigroup.Semigroup, Monoid)

instance Buildable Html where
  type Builder Html = HtmlBuilder
  toBuilder = HtmlBuilder . toBuilder . unHtml
  fromBuilder = Html . fromBuilder . unHtmlBuilder

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
