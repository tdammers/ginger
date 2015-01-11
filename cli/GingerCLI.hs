-- | An example Ginger CLI application.
--
-- Takes two optional arguments; the first one is a template file, the second
-- one a file containing some context data in JSON format.
{-#LANGUAGE OverloadedStrings #-}
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

printF :: GVal (Run IO)
printF = Function $ go
    where
        go :: [(Maybe Text, GVal (Run IO))] -> Run IO (GVal (Run IO))
        go args = forM_ args printArg >> return Null
        printArg (Nothing, String s) = liftRun $ putStrLn (Text.unpack s)
        printArg (Nothing, v) = liftRun $ print v
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
    let contextLookup :: Text -> Run IO (GVal (Run IO))
        contextLookup key =
            case key of
                "print" -> return printF
                _ -> return $ scopeLookup key

    (tpl, src) <- case srcFn of
            Just fn -> (,) <$> parseGingerFile resolve fn <*> return Nothing
            Nothing -> getContents >>= \s -> (,) <$> parseGinger resolve Nothing s <*> return (Just s)

    print tpl

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
        Right t -> runGingerT (makeContextM contextLookup (putStr . Text.unpack . htmlSource)) t

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
