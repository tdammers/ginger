{-#LANGUAGE OverloadedStrings #-}
module Main where

import Text.Ginger
import Text.Ginger.Html
import Data.Text as Text
import Data.Aeson as JSON
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import System.Environment ( getArgs )
import System.IO
import System.IO.Error

main = do
    args <- getArgs

    let scope :: HashMap Text Value
        Just scope = JSON.decode "{ \"name\": \"world\", \"list\": [4, 1, 3] }"
        scopeLookup key = return . fromMaybe Null . HashMap.lookup key $ scope

        loadFile fn = openFile fn ReadMode >>= hGetContents

        resolve fn = tryIOError (loadFile fn) >>= \e ->
                     case e of
                        Right contents -> return (Just contents)
                        Left err -> do
                            print err
                            return Nothing

    (tpl, src) <- case args of
            fn:[] -> (,) <$> parseGingerFile resolve fn <*> return Nothing
            _ -> getContents >>= \s -> (,) <$> parseGinger resolve Nothing s <*> return (Just s)
    case tpl of
        Left err -> do
            printParserError err
            tplSource <- case src of
                            Just s -> return s
                            Nothing -> do
                                let s = peSourceName err
                                case s of
                                    Nothing -> return ""
                                    Just sn -> loadFile sn
            displayParserError tplSource err
        Right t -> runGingerM (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) t

printParserError :: ParserError -> IO ()
printParserError = putStrLn . formatParserError

formatParserError :: ParserError -> String
formatParserError pe = Prelude.concat
    [ fromMaybe "<<unknown source>>" . peSourceName $ pe
    , ":"
    , fromMaybe "" $ (++ ":") . show <$> peSourceLine pe
    , fromMaybe "" $ (++ ":") . show <$> peSourceColumn pe
    , peErrorMessage pe ]

displayParserError :: String -> ParserError -> IO ()
displayParserError src pe = do
    case (peSourceLine pe, peSourceColumn pe) of
        (Just l, cMay) -> do
            let ln = Prelude.take 1 . Prelude.drop (l - 1) . Prelude.lines $ src
            case ln of
                [] -> return ()
                x:_ -> do
                    putStrLn x
                    case cMay of
                        Just c -> putStrLn $ Prelude.replicate (c - 1) ' ' ++ "^"
                        _ -> return ()
        _ -> return ()
