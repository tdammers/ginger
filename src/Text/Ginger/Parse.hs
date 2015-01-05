-- | Ginger parser.
module Text.Ginger.Parse
( parseGinger
, parseGingerFile
, ParserError (..)
, IncludeResolver
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

-- | Used to resolve includes. Ginger will call this function whenever it
-- encounters an {% include %}, {% import %}, or {% extends %} directive.
-- If the required source code is not available, the resolver should return
-- @Nothing@, else @Just@ the source.
type IncludeResolver m = SourceName -> m (Maybe Source)

-- | Parse Ginger source from a file.
parseGingerFile :: Monad m => IncludeResolver m -> SourceName -> m (Either ParserError Template)
parseGingerFile resolve fn = do
    srcMay <- resolve fn
    case srcMay of
        Nothing -> return . Left $
            ParserError
                { peErrorMessage = "Template file not found: " ++ fn
                , peSourceName = Nothing
                , peSourceLine = Nothing
                , peSourceColumn = Nothing
                }
        Just src -> parseGinger resolve src


-- | Parse Ginger source from memory.
parseGinger :: Monad m => IncludeResolver m -> Source -> m (Either ParserError Template)
parseGinger = undefined

-- | Error information for Ginger parser errors.
data ParserError =
    ParserError
        { peErrorMessage :: String -- ^ Human-readable error message
        , peSourceName :: Maybe SourceName -- ^ Source name, if any
        , peSourceLine :: Maybe Int -- ^ Line number, if available
        , peSourceColumn :: Maybe Int -- ^ Column number, if available
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
        (Just $ sourceLine pos)
        (Just $ sourceColumn pos)
