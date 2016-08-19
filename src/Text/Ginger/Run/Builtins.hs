{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Text.Ginger.Run.Builtins
where

import Prelude ( (.), ($), (==), (/=)
               , (>), (<), (>=), (<=)
               , (+), (-), (*), (/), div, (**), (^)
               , (||), (&&)
               , (++)
               , Show, show
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , Int, Integer, String
               , fromIntegral, floor, round
               , not
               , show
               , uncurry
               , seq
               , fst, snd
               , maybe
               , Either (..)
               , id
               , flip
               )
import qualified Prelude
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.List as List
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.GVal
import Text.Ginger.Run.Type
import Text.Ginger.Run.FuncUtils
import Text.Printf
import Text.PrintfA

import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific, formatScientific, FPFormat (Fixed) )
import qualified Data.Scientific as Scientific
import Data.Default (def)
import Safe (readMay, lastDef, headMay)
import Network.HTTP.Types (urlEncode)
import Debug.Trace (trace)
import Data.List (lookup, zipWith, unzip)

gfnRawHtml :: Monad m => Function (Run m h)
gfnRawHtml = unaryFunc (toGVal . unsafeRawHtml . asText)

gfnUrlEncode :: Monad m => Function (Run m h)
gfnUrlEncode =
    unaryFunc
        ( toGVal
        . Text.pack
        . UTF8.toString
        . urlEncode True
        . UTF8.fromString
        . Text.unpack
        . asText
        )

gfnDefault :: Monad m => Function (Run m h)
gfnDefault [] = return def
gfnDefault ((_, x):xs)
    | asBoolean x = return x
    | otherwise = gfnDefault xs

gfnEscape :: Monad m => Function (Run m h)
gfnEscape = return . toGVal . html . mconcat . fmap (asText . snd)

gfnAny :: Monad m => Function (Run m h)
gfnAny xs = return . toGVal $ Prelude.any (asBoolean . snd) xs

gfnAll :: Monad m => Function (Run m h)
gfnAll xs = return . toGVal $ Prelude.all (asBoolean . snd) xs

gfnEquals :: Monad m => Function (Run m h)
gfnEquals [] = return $ toGVal True
gfnEquals [x] = return $ toGVal True
gfnEquals (x:xs) =
    return . toGVal $ Prelude.all ((snd x `looseEquals`) . snd) xs

gfnNEquals :: Monad m => Function (Run m h)
gfnNEquals [] = return $ toGVal True
gfnNEquals [x] = return $ toGVal True
gfnNEquals (x:xs) =
    return . toGVal $ Prelude.any (not . (snd x `looseEquals`) . snd) xs

gfnContains :: Monad m => Function (Run m h)
gfnContains [] = return $ toGVal False
gfnContains (list:elems) =
    let rawList = fromMaybe [] . asList . snd $ list
        rawElems = fmap snd elems
        e `isInList` xs = Prelude.any (looseEquals e) xs
        es `areInList` xs = Prelude.all (`isInList` xs) es
    in return . toGVal $ rawElems `areInList` rawList

looseEquals :: GVal (Run m h) -> GVal (Run m h) -> Bool
looseEquals a b
    | isJust (asFunction a) || isJust (asFunction b) = False
    | isJust (asList a) /= isJust (asList b) = False
    | isJust (asDictItems a) /= isJust (asDictItems b) = False
    -- Both numbers: do numeric comparison
    | isJust (asNumber a) && isJust (asNumber b) = asNumber a == asNumber b
    -- If either is NULL, the other must be falsy
    | isNull a || isNull b = asBoolean a == asBoolean b
    | otherwise = asText a == asText b

gfnLess :: Monad m => Function (Run m h)
gfnLess [] = return . toGVal $ False
gfnLess xs' =
    let xs = fmap snd xs'
    in return . toGVal $
        Prelude.all (== Just True) (Prelude.zipWith less xs (Prelude.tail xs))

gfnGreater :: Monad m => Function (Run m h)
gfnGreater [] = return . toGVal $ False
gfnGreater xs' =
    let xs = fmap snd xs'
    in return . toGVal $
        Prelude.all (== Just True) (Prelude.zipWith greater xs (Prelude.tail xs))

gfnLessEquals :: Monad m => Function (Run m h)
gfnLessEquals [] = return . toGVal $ False
gfnLessEquals xs' =
    let xs = fmap snd xs'
    in return . toGVal $
        Prelude.all (== Just True) (Prelude.zipWith lessEq xs (Prelude.tail xs))

gfnGreaterEquals :: Monad m => Function (Run m h)
gfnGreaterEquals [] = return . toGVal $ False
gfnGreaterEquals xs' =
    let xs = fmap snd xs'
    in return . toGVal $
        Prelude.all (== Just True) (Prelude.zipWith greaterEq xs (Prelude.tail xs))

less :: Monad m => GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
less a b = (<) <$> asNumber a <*> asNumber b

greater :: Monad m => GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
greater a b = (>) <$> asNumber a <*> asNumber b

lessEq :: Monad m => GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
lessEq a b = (<=) <$> asNumber a <*> asNumber b

greaterEq :: Monad m => GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
greaterEq a b = (>=) <$> asNumber a <*> asNumber b

difference :: Prelude.Num a => [a] -> a
difference (x:xs) = x - Prelude.sum xs
difference [] = 0

ratio :: (Show a, Prelude.Fractional a, Prelude.Num a) => [a] -> a
ratio (x:xs) = x / Prelude.product xs
ratio [] = 0

intRatio :: (Prelude.Integral a, Prelude.Num a) => [a] -> a
intRatio (x:xs) = x `Prelude.div` Prelude.product xs
intRatio [] = 0

modulo :: (Prelude.Integral a, Prelude.Num a) => [a] -> a
modulo (x:xs) = x `Prelude.mod` Prelude.product xs
modulo [] = 0

capitalize :: Text -> Text
capitalize txt = Text.toUpper (Text.take 1 txt) <> Text.drop 1 txt

gfnCenter :: Monad m => Function (Run m h)
gfnCenter [] = gfnCenter [(Nothing, toGVal ("" :: Text))]
gfnCenter [x] = gfnCenter [x, (Nothing, toGVal (80 :: Int))]
gfnCenter [x,y] = gfnCenter [x, y, (Nothing, toGVal (" " :: Text))]
gfnCenter ((_, s):(_, w):(_, pad):_) =
    return . toGVal $ center (asText s) (fromMaybe 80 $ Prelude.truncate <$> asNumber w) (asText pad)

gfnSlice :: Monad m => Function (Run m h)
gfnSlice args =
    let argValues =
            extractArgsDefL
                [ ("slicee", def)
                , ("start", def)
                , ("length", def)
                ]
                args
    in case argValues of
        Right [slicee, startPos, length] -> do
            let startInt :: Int
                startInt = maybe 0 Prelude.round . asNumber $ startPos

                lengthInt :: Maybe Int
                lengthInt = fmap Prelude.round . asNumber $ length

                slice :: [a] -> Int -> Maybe Int -> [a]
                slice xs start Nothing
                    | start < 0 =
                        Prelude.drop (Prelude.length xs + start) xs
                    | otherwise =
                        Prelude.drop start xs
                slice xs start (Just length) =
                    Prelude.take length $ slice xs start Nothing
            case asDictItems slicee of
                Just items -> do
                    let slicedItems = slice items startInt lengthInt
                    return $ dict slicedItems
                Nothing ->
                    case asList slicee of
                    Just items ->
                        return . toGVal $ slice items startInt lengthInt
                    Nothing ->
                        return . toGVal . Text.pack $
                            slice (Text.unpack $ asText slicee) startInt lengthInt
        _ -> fail "Invalid arguments to 'slice'"

gfnReplace :: Monad m => Function (Run m h)
gfnReplace args =
    let argValues =
            extractArgsDefL
                [ ("str", def)
                , ("search", def)
                , ("replace", def)
                ]
                args
    in case argValues of
        Right [strG, searchG, replaceG] -> do
            let str = asText strG
                search = asText searchG
                replace = asText replaceG
            return . toGVal $ Text.replace search replace str
        _ -> fail "Invalid arguments to 'replace'"

gfnSort :: Monad m => Function (Run m h)
gfnSort args = do
    let parsedArgs = extractArgsDefL
            [ ("sortee", def)
            , ("by", def)
            , ("reverse", toGVal False)
            ]
            args
    (sortee, sortKey, sortReverse) <- case parsedArgs of
        Right [sortee, sortKeyG, reverseG] ->
            return ( sortee
                   , asText sortKeyG
                   , asBoolean reverseG
                   )
        _ ->
            fail "Invalid args to sort()"
    let baseComparer :: GVal (Run m h) -> GVal (Run m h) -> Prelude.Ordering
        baseComparer a b = Prelude.compare (asText a) (asText b)
        extractKey :: Text -> GVal (Run m h) -> GVal (Run m h)
        extractKey k g = fromMaybe def $ do
            l <- asLookup g
            l k
    if isDict sortee
        then do
            let comparer' :: (Text, GVal (Run m h)) -> (Text, GVal (Run m h)) -> Prelude.Ordering
                comparer' = case sortKey of
                    "" -> \(_, a) (_, b) -> baseComparer a b
                    "__key" -> \(a, _) (b, _) -> Prelude.compare a b
                    k -> \(_, a) (_, b) ->
                        baseComparer
                            (extractKey k a) (extractKey k b)
                comparer =
                    if sortReverse
                        then flip comparer'
                        else comparer'
            return . toGVal $ List.sortBy comparer (fromMaybe [] $ asDictItems sortee)
        else do
            let comparer' :: GVal (Run m h) -> GVal (Run m h) -> Prelude.Ordering
                comparer' = case sortKey of
                    "" ->
                        baseComparer
                    k -> \a b ->
                        baseComparer
                            (extractKey k a) (extractKey k b)
            let comparer =
                    if sortReverse
                        then flip comparer'
                        else comparer'
            return . toGVal $ List.sortBy comparer (fromMaybe [] $ asList sortee)

center :: Text -> Prelude.Int -> Text -> Text
center str width pad =
    if Text.length str Prelude.>= width
        then str
        else paddingL <> str <> paddingR
    where
        chars = width - Text.length str
        charsL = chars `div` 2
        charsR = chars - charsL
        repsL = Prelude.succ charsL `div` Text.length pad
        paddingL = Text.take charsL . Text.replicate repsL $ pad
        repsR = Prelude.succ charsR `div` Text.length pad
        paddingR = Text.take charsR . Text.replicate repsR $ pad

gfnFileSizeFormat :: Monad m => Function (Run m h)
gfnFileSizeFormat [(_, sizeG)] =
    gfnFileSizeFormat [(Nothing, sizeG), (Nothing, def)]
gfnFileSizeFormat [(_, sizeG), (_, binaryG)] = do
    let sizeM = Prelude.round <$> asNumber sizeG
        binary = asBoolean binaryG
    Prelude.maybe
        (return def)
        (return . toGVal . formatFileSize binary)
        sizeM
gfnFileSizeFormat _ = return def

formatFileSize :: Bool -> Integer -> String
formatFileSize binary size =
    let units =
            if binary
                then
                    [ (1, "B")
                    , (1024, "kiB")
                    , (1024 ^ 2, "MiB")
                    , (1024 ^ 3, "GiB")
                    , (1024 ^ 4, "TiB")
                    , (1024 ^ 5, "PiB")
                    ]
                else
                    [ (1, "B")
                    , (1000, "kB")
                    , (1000000, "MB")
                    , (1000000000, "GB")
                    , (1000000000000, "TB")
                    , (1000000000000000, "PB")
                    ]
        (divisor, unitName) =
            lastDef (1, "B") [ (d, u) | (d, u) <- units, d <= size ]
        dividedSize :: Scientific
        dividedSize = fromIntegral size / fromIntegral divisor
        formattedSize =
            if Scientific.isInteger dividedSize
                then formatScientific Fixed (Just 0) dividedSize
                else formatScientific Fixed (Just 1) dividedSize
    in formattedSize ++ " " ++ unitName

gfnPrintf :: Monad m => Function (Run m h)
gfnPrintf [] = return def
gfnPrintf [(_, fmtStrG)] = return fmtStrG
gfnPrintf ((_, fmtStrG):args) = do
    return . toGVal $ printfG fmtStr (fmap snd args)
    where
        fmtStr = Text.unpack $ asText fmtStrG

gfnFilter :: Monad m => Function (Run m h)
gfnFilter [] = return def
gfnFilter [(_, xs)] = return xs
gfnFilter ((_, xs):(_, p):args) = do
    pfnG <- maybe (fail "Not a function") return (asFunction p)
    let pfn x = asBoolean <$> pfnG ((Nothing, x):args)
        xsl = fromMaybe [] (asList xs)
    filtered <- filterM pfn xsl
    return $ toGVal filtered

printfG :: String -> [GVal m] -> String
printfG fmt args = printfa fmt (fmap P args)

