-- | Ginger parser.
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

-- | Input type for the parser (source code).
type Source = String

-- | A source identifier (typically a filename).
type SourceName = String

-- | Parse Ginger source from a file.
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


-- | Parse Ginger source from memory.
parseGinger :: Monad m => IncludeResolver m -> Source -> m (Either ParserError Template)
parseGinger = undefined

type IncludeResolver m = String -> m (Maybe String)

-- | Error information for Ginger parser errors.
data ParserError =
    ParserError
        { peErrorMessage :: String -- ^ Human-readable error message
        , peSourceName :: Maybe SourceName -- ^ Source name, if any
        , peSourceLine :: Int -- ^ Line number, if available
        , peSourceColumn :: Int -- ^ Column number, if available
        }
        deriving (Show)

-- | Helper function to create a Ginger parser error from a Parsec error.
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
