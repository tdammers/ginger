{-#LANGUAGE OverloadedStrings #-}
module Main where

import Text.Ginger
import Text.Ginger.Html
import Data.Text as Text
import Data.Aeson as JSON
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

main = do
    let scope :: HashMap Text Value
        Just scope = JSON.decode "{ \"name\": \"world\", \"list\": [4, 1, 3] }"
        scopeLookup key = return . fromMaybe Null . HashMap.lookup key $ scope

        resolve _ = return Nothing

    tplSource <- getContents
    tpl <- parseGinger resolve "input" tplSource
    case tpl of
        Left err -> print err
        Right t -> runGingerM (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) t
