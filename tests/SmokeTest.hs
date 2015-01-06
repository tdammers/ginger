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

main = do
    let scope :: HashMap Text Value
        Just scope = JSON.decode "{ \"name\": \"world\", \"list\": [4, 1, 3] }"
        scopeLookup key = return . fromMaybe Null . HashMap.lookup key $ scope

        resolve _ = return Nothing

    tplSource <- getContents
    tpl <- parseGinger resolve "input" tplSource
    case tpl of
        Left err -> do
            printParserError err
            displayParserError tplSource err
        Right t -> runGingerM (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) t

printParserError :: ParserError -> IO ()
printParserError pe = do
    putStr . fromMaybe "<<unknown source>>" . peSourceName $ pe
    putStr ":"
    putStr . fromMaybe "" $ (++ ":") . show <$> peSourceLine pe
    putStr . fromMaybe "" $ (++ ":") . show <$> peSourceColumn pe
    putStrLn $ peErrorMessage pe

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
