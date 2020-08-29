{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeFamilies #-}

module Text.Ginger.Buildable
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText

class Buildable s where
  type Builder s
  toBuilder :: s -> Builder s
  fromBuilder :: Builder s -> s

instance Buildable Text where
  type Builder Text = LText.Builder
  toBuilder = LText.fromText
  fromBuilder = LText.toStrict . LText.toLazyText

instance Buildable LText.Text where
  type Builder LText.Text = LText.Builder
  toBuilder = LText.fromLazyText
  fromBuilder = LText.toLazyText

instance Buildable [a] where
  type Builder [a] = [a]
  toBuilder = id
  fromBuilder = id

