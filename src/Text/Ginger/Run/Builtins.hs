{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
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
import Text.Ginger.Run.VM
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
import Data.Time ( defaultTimeLocale
                 , formatTime
                 , LocalTime (..)
                 , ZonedTime (..)
                 , utc
                 , utcToZonedTime
                 , zonedTimeToUTC
                 , TimeOfDay (..)
                 , fromGregorian
                 , Day (..)
                 , parseTimeM
                 , TimeLocale (..)
                 , TimeZone (..)
                 )
import Data.Foldable (asum)

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

gfnMap :: Monad m => Function (Run m h)
gfnMap args = do
    let parsedArgs = extractArgsDefL
            [ ("collection", def)
            , ("function", def)
            , ("attribute", def)
            ]
            args
    (dictMay, listMay, functionMay, attributeMay) <- case parsedArgs of
        Right [collection, function, attribute] ->
            return ( asDictItems collection
                   , asList collection
                   , asFunction function
                   , Just (asText attribute)
                   )
        _ ->
            fail "Invalid args to map()"
    mapFunction <- case (functionMay, attributeMay) of
        (Just f, _) -> return f
        (Nothing, Just key) -> return $ \case
            (_, item):_ ->
                return $ lookupLooseDef def (toGVal key) item
            _ -> 
                return def
        _ -> fail "You have to pass a function or an attribute"
    case (dictMay, listMay) of
        (Just items, _) ->
            dict <$> forM items
                (\(key, value) -> (key,) <$> mapFunction [(Nothing, value)])
        (Nothing, Just items) ->
            toGVal <$> mapM (mapFunction . (:[]) . (Nothing,)) items
            

gfnSort :: Monad m => Function (Run m h)
gfnSort args = do
    let parsedArgs = extractArgsDefL
            [ ("sortee", def)
            , ("by", def)
            , ("reverse", toGVal False)
            ]
            args
    (sortee, sortKey, sortReverse) <- case parsedArgs of
        Right [sortee, sortKey, reverseG] ->
            return ( sortee
                   , sortKey
                   , asBoolean reverseG
                   )
        _ ->
            fail "Invalid args to sort()"
    let -- extractByFunc :: Maybe ((Text, GVal (Run m h)) -> Run m h (GVal (Run m h)))
        extractByFunc = do
            f <- asFunction sortKey
            return $ \g ->
                f [(Nothing, snd g)]

    let -- extractByPath :: Maybe ((Text, GVal (Run m h)) -> Run m h (GVal (Run m h)))
        extractByPath = do
            keys <- asList sortKey
            return $ \g ->
                return $ List.foldl' (flip (lookupLooseDef def)) (snd g) keys

        extractByKey :: Monad m => Maybe ((Text, a) -> Run m h (GVal (Run m h)))
        extractByKey =
            if asText sortKey == "__key"
                then Just $ return . toGVal . fst
                else Nothing

        extractByProp =
            if isNull sortKey
                then Nothing
                else return $ return . lookupLooseDef def sortKey . snd

        extractFunc = fromMaybe (return . snd) $
            extractByFunc <|>
            extractByPath <|>
            extractByKey <|>
            extractByProp

        (pack, unpacked) =
            case (asDictItems sortee, asList sortee) of
                (Just dictItems, _) ->
                    (orderedDict, dictItems)
                (Nothing, Just listItems) ->
                    (toGVal . fmap snd, listToIndexedDict listItems)
                (Nothing, Nothing) ->
                    (Prelude.const def, [])

    tagged <- forM unpacked $ \item -> do
        extracted <- extractFunc item
        return (asText extracted, item)
    let compare =
            onFst $ if sortReverse
                then flip Prelude.compare
                else Prelude.compare

    let sorted = pack . fmap snd $ List.sortBy compare tagged

    return sorted

onFst :: (a -> a -> b) -> (a, c) -> (a, d) -> b
onFst f (x, _) (y, _) = f x y

listToIndexedDict :: [a] -> [(Text, a)]
listToIndexedDict values =
    let indexes = fmap (Text.pack . show) [0..]
    in List.zip indexes values

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

gvalToDate :: TimeZone -> GVal m -> Maybe ZonedTime
gvalToDate tz g = gvalDictToDate tz g
             <|> gvalListToDate tz g
             <|> gvalAutoParseDate tz g

gvalDictToDate :: TimeZone -> GVal m -> Maybe ZonedTime
gvalDictToDate defTZ g = do
    let datePartMay = do
            year <- fmap (fromIntegral :: Int -> Integer) $ g ~! "year"
            month <- g ~! "month"
            day <- g ~! "day"
            return $ fromGregorian year month day
        timePartMay = do
            hours <- g ~! "hours"
            minutes <- g ~! "minutes"
            seconds <- fmap scientificToPico $ g ~! "seconds"
            return $ TimeOfDay hours minutes seconds
        tzPartMay = g ~! "tz"
    when (isNothing datePartMay && isNothing timePartMay) Nothing
    let datePart = fromMaybe (fromGregorian 1970 1 1) datePartMay
        timePart = fromMaybe (TimeOfDay 12 0 0) timePartMay
        tz = fromMaybe defTZ tzPartMay
    return $ ZonedTime (LocalTime datePart timePart) tz

gvalListToDate :: TimeZone -> GVal m -> Maybe ZonedTime
gvalListToDate defTZ g = go =<< asList g
    where
        go :: [GVal m] -> Maybe ZonedTime
        go parts = case parts of
            [yearG, monthG, dayG, hoursG, minutesG, secondsG, tzG] -> do
                datePart <-
                    fromGregorian
                        <$> (fromIntegral <$> toInt yearG)
                        <*> toInt monthG
                        <*> toInt dayG
                timePart <-
                    TimeOfDay
                        <$> toInt hoursG
                        <*> toInt minutesG
                        <*> (fromIntegral <$> toInt secondsG)
                tzPart <- fromGVal tzG
                return $ ZonedTime (LocalTime datePart timePart) tzPart
            [yearG, monthG, dayG, hoursG, minutesG, secondsG] ->
                go [yearG, monthG, dayG, hoursG, minutesG, secondsG, toGVal defTZ]
            [yearG, monthG, dayG, hoursG, minutesG] ->
                go [yearG, monthG, dayG, hoursG, minutesG, toGVal (0 :: Int)]
            [yearG, monthG, dayG] ->
                go [yearG, monthG, dayG, toGVal (12 :: Int), toGVal (0 :: Int)]
            _ -> Nothing

gvalAutoParseDate :: TimeZone -> GVal m -> Maybe ZonedTime
gvalAutoParseDate defTZ = go . Text.unpack . asText
    where
        go input = asum [ parse t input | (parse, t) <- formats ]
        ztparse :: String -> String -> Maybe ZonedTime
        ztparse fmt = parseTimeM True defaultTimeLocale fmt
        utcparse :: String -> String -> Maybe ZonedTime
        utcparse fmt input = do
            lt <- parseTimeM True defaultTimeLocale fmt input
            return $ ZonedTime lt defTZ
        formats =
            [ (ztparse, "%Y-%m-%dT%H:%M:%S%Z")
            , (utcparse, "%Y-%m-%d %H:%M:%S")
            , (ztparse, "%Y-%m-%d %H:%M:%S%z")
            , (ztparse, "%Y-%m-%d %H:%M:%S%Z")
            , (utcparse, "%Y-%m-%d")
            ]

gvalToTZ :: GVal m -> Maybe TimeZone
gvalToTZ g =
    fromGVal g <|> (parseTZ . Text.unpack . asText $ g)

parseTZ :: String -> Maybe TimeZone
parseTZ = parseTimeM True defaultTimeLocale "%z"

gfnDateFormat :: Monad m => Function (Run m h)
gfnDateFormat args =
    let extracted =
            extractArgsDefL
                [ ("date", def)
                , ("format", def)
                , ("tz", def)
                , ("locale", def)
                ]
                args
    in case extracted of
        Right [gDate, gFormat, gTimeZone, gLocale] ->
                -- The desired target timezone; Nothing means keep original timezone
            let tzMay = gvalToTZ gTimeZone
                -- The default timezone used when the input doesn't include timezone
                -- information; if a target timezone is given, then it is also used as
                -- the default, otherwise, UTC is assumed. The underlying assumptions
                -- are:
                --
                -- * If the input does not include timezone information, then it is a
                --   local time; hence, if the user explicitly passes a time zone for
                --   formatting, it is assumed that this means the original local time
                --   is in that time zone.
                -- * If the input does not include timezone information, and no
                --   explicit timezone is given, the only sane time zone to pick is
                --   UTC. In this situation, the incoming dates either originate from
                --   a system that doesn't track timezone information but implicitly
                --   stores everything in UTC (which is fine), or the formatting
                --   doesn't use timezone information anyway (in which case it doesn't
                --   matter), or the originator of the data uses another timezone but
                --   fails to report it (in which case it is impossible to do the right
                --   thing)
                -- * If the input *does* include timezone information, it should be
                --   respected; explicitly passing timezone information in the date()
                --   call means the user wants to represent the same zoned time in a
                --   different time zone, which means time zone conversion is required.
                defTZ = fromMaybe utc tzMay
                dateMay = gvalToDate defTZ gDate
                fmtMay = Text.unpack <$> fromGVal gFormat
            in case fmtMay of
                Just fmt -> do
                    locale <- maybe
                        (getTimeLocale gLocale)
                        return
                        (fromGVal gLocale)
                    return . toGVal $ formatTime locale fmt . convertTZ tzMay <$> dateMay
                Nothing -> do
                    return . toGVal $ convertTZ tzMay <$> dateMay
        _ -> fail "Invalid arguments to 'date'"
    where
        convertTZ :: Maybe TimeZone -> ZonedTime -> ZonedTime
        convertTZ Nothing = id
        convertTZ (Just tz) = utcToZonedTime tz . zonedTimeToUTC

getTimeLocale :: Monad m => GVal (Run m h) -> Run m h TimeLocale
getTimeLocale localeName = do
    toFunction <$> getVar "getlocale" >>= \case
        Nothing ->
            return defaultTimeLocale
        Just getlocale -> do
            let args = [ (Just "category", "LC_TIME")
                       , (Just "locale", localeName)
                       ]
            fromMaybe defaultTimeLocale . fromGVal <$> getlocale args


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


gfnDictsort :: Monad m => Function (Run m h)
gfnDictsort args =
    let extracted =
            extractArgsDefL
                [ ("dict", def)
                , ("case_sensitive", def)
                , ("by", "key")
                ]
                args
    in case extracted of
        Right [gDict, gCaseSensitive, gSortBy] -> do
            let caseSensitive = asBoolean gCaseSensitive
            sortByKey <- case asText gSortBy of
                "key" -> return True
                "value" -> return False
                "val" -> return False
                x -> fail $ "Invalid value for 'dictsort()' argument 'by': " ++ show x
            let items = fromMaybe [] $ asDictItems gDict
            let projection =
                    (if caseSensitive then id else Text.toUpper) .
                    (if sortByKey then fst else (asText . snd))
            return . orderedDict . List.sortOn projection $ items
        _ -> fail "Invalid arguments to 'dictsort'"
