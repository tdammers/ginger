{-#LANGUAGE OverloadedStrings #-}
module Main where

import Text.Ginger
import Text.Ginger.Html
import Text.Ginger.Value.Text
import Data.Text as Text

main = do
    let scope :: VarName -> IO Text
        scope "hello" = return "hello"
        scope _ = return ""

        resolve _ = return Nothing

    tplSource <- getContents
    tpl <- parseGinger resolve "input" tplSource
    case tpl of
        Left err -> print err
        Right t -> runGingerM (makeContextM scope (putStr . Text.unpack . htmlSource)) t
