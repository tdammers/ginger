{-#LANGUAGE OverloadedStrings #-} module Text.Ginger.PropertyTests
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)

import Text.Ginger
import Text.Ginger.Html

instance Arbitrary Text where
    arbitrary = Text.pack <$> arbitrary

instance Arbitrary Html where
    arbitrary = oneof
        [ html <$> arbitrary
        , arbitraryTag
        ]

arbitraryTag = do
    tagName <- arbitrary
    inner <- arbitrary
    return $ mconcat
        [ unsafeRawHtml "<"
        , tagName
        , unsafeRawHtml ">"
        , html inner
        , unsafeRawHtml "</"
        , tagName
        , unsafeRawHtml ">"
        ]

instance Arbitrary Statement where
    arbitrary = arbitraryStatement 2

arbitrarySimpleStatement = oneof
        [ return NullS
        , ScopedS <$> arbitrary
        , LiteralS <$> arbitrary
        -- , InterpolationS <$> arbitrary
        -- , IfS <$> arbitrary <*> arbitrary <*> arbitrary
        -- , ForS <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        -- , SetVarS <$> arbitrary <*> arbitrary
        -- , DefMacroS <$> arbitrary <*> arbitrary
        -- , BlockRefS <$> arbitrary
        -- , PreprocessedIncludeS <$> arbitrary
        ]

arbitraryStatement :: Int -> Gen Statement
arbitraryStatement 0 = arbitrarySimpleStatement
arbitraryStatement n = oneof
    [ arbitrarySimpleStatement
    , MultiS <$> resize 5 (listOf $ arbitraryStatement (pred n))
    ]

instance Arbitrary Template where
    arbitrary =
        Template <$> arbitrary <*> return HashMap.empty <*> return Nothing

propertyTests :: TestTree
propertyTests = testGroup "Properties"
    [ testProperty "optimizer doesn't change behavior" $
        \ast -> unsafePerformIO $ (==) <$> expand ast <*> expand (optimize ast)
    ]

expand :: Template -> IO (Either String Text)
expand tpl =
    mapLeft (const "ERROR" :: SomeException -> String) <$>
        try (return $ runGinger (makeContextText (const def)) tpl)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft f (Right x) = Right x
