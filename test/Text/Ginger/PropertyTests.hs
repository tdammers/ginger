{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
module Text.Ginger.PropertyTests
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Identity (Identity)
import Data.Time

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

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary TimeOfDay where
    arbitrary =
        TimeOfDay
            <$> resize 24 arbitrarySizedNatural
            <*> resize 60 arbitrarySizedNatural
            <*> (fromIntegral <$> resize 61 arbitrarySizedNatural)

instance Arbitrary LocalTime where
    arbitrary =
        LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary TimeZone where
    arbitrary =
        TimeZone <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ZonedTime where
    arbitrary =
        ZonedTime <$> arbitrary <*> arbitrary

instance Arbitrary (Statement ()) where
    arbitrary = arbitraryStatement 2

arbitrarySimpleStatement = oneof
        [ return (NullS ())
        , ScopedS () <$> arbitrary
        , LiteralS () <$> arbitrary
        -- , InterpolationS <$> arbitrary
        -- , IfS <$> arbitrary <*> arbitrary <*> arbitrary
        -- , ForS <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        -- , SetVarS <$> arbitrary <*> arbitrary
        -- , DefMacroS <$> arbitrary <*> arbitrary
        -- , BlockRefS <$> arbitrary
        -- , PreprocessedIncludeS <$> arbitrary
        ]

arbitraryStatement :: Int -> Gen (Statement ())
arbitraryStatement 0 = arbitrarySimpleStatement
arbitraryStatement n = oneof
    [ arbitrarySimpleStatement
    , MultiS () <$> resize 5 (listOf $ arbitraryStatement (pred n))
    ]

instance Arbitrary (Template ()) where
    arbitrary =
        Template <$> arbitrary <*> return HashMap.empty <*> return Nothing

propertyTests :: TestTree
propertyTests = testGroup "Properties"
    [ testGroup "Optimizer" $
        [ testProperty "optimizer doesn't change behavior" $
            \ast -> unsafePerformIO $ (==) <$> expand ast <*> expand (optimize ast)
        ]
    , testGroup "ToGVal / FromGVal round tripping"
        [ testProperty "Int" (roundTripGValP :: Int -> Bool)
        , testProperty "Bool" (roundTripGValP :: Bool -> Bool)
        , testProperty "[Text]" (roundTripGValP :: [Text] -> Bool)
        , testProperty "Maybe Text" (roundTripGValP :: Maybe Text -> Bool)
        , testProperty "Text" (roundTripGValP :: Text -> Bool)
        , testProperty "LocalTime" (roundTripGValP :: LocalTime -> Bool)
        , testProperty "TimeZone" (roundTripGValP :: TimeZone -> Bool)
        -- For ZonedTime, we don't have an Eq instance because it equality
        -- of datetimes across time zones is unsolvable; we can, however, use
        -- a "strict" equality test by simply rendering zoned times through
        -- 'show', i.e., we consider two ZonedTime values equal iff they render
        -- to the exact same string representations.
        , testProperty "ZonedTime" (roundTripGValProjP show :: ZonedTime -> Bool)
        ]
    ]

roundTripGValExP :: (ToGVal Identity a, FromGVal Identity a)
               => (a -> a -> Bool)
               -> a
               -> Bool
roundTripGValExP cmp orig =
    let g :: GVal Identity
        g = toGVal orig
    in case fromGVal g of
        Nothing -> False
        Just final -> cmp orig final

roundTripGValProjP :: (Eq b, ToGVal Identity a, FromGVal Identity a)
               => (a -> b)
               -> a
               -> Bool
roundTripGValProjP
    proj = roundTripGValExP f
    where
        f x y = proj x == proj y

roundTripGValP :: (Eq a, ToGVal Identity a, FromGVal Identity a)
               => a -> Bool
roundTripGValP = roundTripGValExP (==)

expand :: Template () -> IO (Either String Text)
expand tpl =
    mapLeft (const "ERROR" :: SomeException -> String) <$>
        try (return $ runGinger (makeContextText (const def)) tpl)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft f (Right x) = Right x
