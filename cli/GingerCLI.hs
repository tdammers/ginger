-- | An example Ginger CLI application.
--
-- Takes two optional arguments; the first one is a template file, the second
-- one a file containing some context data in JSON format.
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Ginger
import Text.Ginger.Html
import Data.Text as Text
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import System.Environment ( getArgs )
import System.IO
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Default ( def )

loadFile fn = openFile fn ReadMode >>= hGetContents

loadFileMay fn =
    tryIOError (loadFile fn) >>= \e ->
         case e of
            Right contents -> return (Just contents)
            Left err -> do
                print err
                return Nothing

decodeFile :: (JSON.FromJSON v) => FilePath -> IO (Maybe v)
decodeFile fn = JSON.decode <$> (openFile fn ReadMode >>= LBS.hGetContents)

printF :: GVal (Run p IO Html)
printF = fromFunction $ go
    where
        go :: [(Maybe Text, GVal (Run p IO Html))] -> Run p IO Html (GVal (Run p IO Html))
        go args = forM_ args printArg >> return def
        printArg (Nothing, v) = liftRun . putStrLn . Text.unpack . asText $ v
        printArg (Just x, _) = return ()

main = do
    args <- getArgs
    let (srcFn, scopeFn) = case args of
            [] -> (Nothing, Nothing)
            a:[] -> (Just a, Nothing)
            a:b:[] -> (Just a, Just b)

    scope <- case scopeFn of
        Nothing -> return Nothing
        Just fn -> (decodeFile fn :: IO (Maybe (HashMap Text JSON.Value)))

    let scopeLookup key = toGVal (scope >>= HashMap.lookup key)
        resolve = loadFileMay
    let contextLookup :: Text -> Run p IO Html (GVal (Run p IO Html))
        contextLookup key =
            case key of
                "print" -> return printF
                _ -> return $ scopeLookup key

    (tpl, src) <- case srcFn of
            Just fn -> (,) <$> parseGingerFile resolve fn <*> return Nothing
            Nothing -> getContents >>= \s -> (,) <$> parseGinger resolve Nothing s <*> return (Just s)

    -- TODO: do some sort of arg parsing thing so that we can turn
    -- template dumping on or off.
    -- print tpl

    case tpl of
        Left err -> do
            tplSource <-
                case src of
                    Just s ->
                        return (Just s)
                    Nothing -> do
                        let s = sourceName <$> peSourcePosition err
                        case s of
                            Nothing -> return Nothing
                            Just sn -> Just <$> loadFile sn
            printParserError tplSource err
        Right t -> do
            let context =
                    makeContextHtmlExM
                        contextLookup
                        (putStr . Text.unpack . htmlSource)
                        (hPutStrLn stderr . show)
            runGingerT context t >>= either (hPutStrLn stderr . show) showOutput
    where
      showOutput value
        | isNull value = return ()
        | otherwise = putStrLn . show $ value

printParserError :: Maybe String -> ParserError -> IO ()
printParserError srcMay = putStrLn . formatParserError srcMay

displayParserError :: String -> ParserError -> IO ()
displayParserError src pe = do
    case peSourcePosition pe of
        Just pos -> do
            let ln = Prelude.take 1 . Prelude.drop (sourceLine pos - 1) . Prelude.lines $ src
            case ln of
                [] -> return ()
                x:_ -> do
                    putStrLn x
                    putStrLn $ Prelude.replicate (sourceColumn pos - 1) ' ' ++ "^"
        _ -> return ()
