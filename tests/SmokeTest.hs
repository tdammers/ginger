{-#LANGUAGE OverloadedStrings #-}
module Main where

import Text.Ginger
import Text.Ginger.Html
import Text.Ginger.Value.JSON
import Data.Text as Text
import Data.Aeson as JSON

main = do
    let scope :: VarName -> IO JSON.Value
        scope "hello" = return $ String "hello"
        scope "str" = return $ String "This is a string"
        scope "true" = return $ Bool True
        scope "false" = return $ Bool False
        scope "int" = return $ Number 23
        scope "float" = return $ Number 4.5
        scope "null" = return Null
        scope _ = return Null

        resolve _ = return Nothing

    tplSource <- getContents
    tpl <- parseGinger resolve "input" tplSource
    case tpl of
        Left err -> print err
        Right t -> runGingerM (makeContextM scope (putStr . Text.unpack . htmlSource)) t
