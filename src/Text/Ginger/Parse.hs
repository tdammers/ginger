module Text.Ginger.Parse
( parseGinger
, parseGingerFile
, ParserError
)
where

import Text.Parsec ( ParseError
                   , sourceLine
                   , sourceColumn
                   , sourceName
                   )
import Text.Parsec.Error ( errorMessages
                         , errorPos
                         , messageString
                         )
import Text.Ginger.AST

type Source = String
type SourceName = String

parseGingerFile :: Monad m => IncludeResolver m -> SourceName -> m (Either ParserError Template)
parseGingerFile resolve fn = do
    srcMay <- resolve fn
    case srcMay of
        Nothing -> return . Left $
            ParserError
                { peErrorMessage = "Template file not found: " ++ fn
                , peSourceFile = Nothing
                , peSourceLine = 0
                , peSourceColumn = 0
                }
        Just src -> parseGinger resolve src


parseGinger :: Monad m => IncludeResolver m -> Source -> m (Either ParserError Template)
parseGinger = undefined

type IncludeResolver m = String -> m (Maybe String)

data ParserError =
    ParserError
        { peErrorMessage :: String
        , peSourceFile :: Maybe String
        , peSourceLine :: Int
        , peSourceColumn :: Int
        }
        deriving (Show)

fromParsecError :: ParseError -> ParserError
fromParsecError e =
    let pos = errorPos e
        sourceFilename =
            let sn = sourceName pos
            in if null sn then Nothing else Just sn
    in ParserError
        (unlines . map messageString . errorMessages $ e)
        sourceFilename
        (sourceLine pos)
        (sourceColumn pos)
