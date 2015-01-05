-- | Ginger parser.
module Text.Ginger.Parse
( parseGinger
, parseGingerFile
, ParserError (..)
, IncludeResolver
, Source, SourceName
)
where

import Text.Parsec ( ParseError
                   , sourceLine
                   , sourceColumn
                   , sourceName
                   , ParsecT
                   , runParserT
                   , manyTill, oneOf, string, notFollowedBy, try, lookAhead
                   , eof, spaces, anyChar, char
                   , unexpected
                   )
import Text.Parsec.Error ( errorMessages
                         , errorPos
                         , showErrorMessages
                         )
import Text.Ginger.AST
import Text.Ginger.Html ( unsafeRawHtml )
import Control.Monad.Reader ( ReaderT
                            , runReaderT
                            )
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text

-- | Input type for the parser (source code).
type Source = String

-- | A source identifier (typically a filename).
type SourceName = String

-- | Used to resolve includes. Ginger will call this function whenever it
-- encounters an {% include %}, {% import %}, or {% extends %} directive.
-- If the required source code is not available, the resolver should return
-- @Nothing@, else @Just@ the source.
type IncludeResolver m = SourceName -> m (Maybe Source)


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
        (dropWhile (== '\n') .
            showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            $ errorMessages e)
        sourceFilename
        (Just $ sourceLine pos)
        (Just $ sourceColumn pos)
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
        Just src -> parseGinger resolve fn src


data ParseContext m
    = ParseContext
        { pcResolve :: IncludeResolver m
        }

-- | Parse Ginger source from memory.
parseGinger :: Monad m => IncludeResolver m -> SourceName -> Source -> m (Either ParserError Template)
parseGinger resolve sn src = do
    result <- runReaderT (runParserT templateP () sn src) (ParseContext resolve)
    case result of
        Right t -> return . Right $ t
        Left e -> return . Left $ fromParsecError e

type Parser m a = ParsecT String () (ReaderT (ParseContext m) m) a

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

templateP :: Monad m => Parser m Template
templateP = do
    Template <$> statementsP

statementsP :: Monad m => Parser m Statement
statementsP = do
    stmts <- many statementP
    eof
    case stmts of
        [] -> return NullS
        x:[] -> return x
        xs -> return $ MultiS xs

statementP :: Monad m => Parser m Statement
statementP = interpolationStmtP
           <|> literalStmtP

interpolationStmtP :: Monad m => Parser m Statement
interpolationStmtP = do
    try $ string "{{"
    spaces
    expr <- expressionP
    spaces
    string "}}"
    return $ InterpolationS expr

literalStmtP :: Monad m => Parser m Statement
literalStmtP = do
    txt <- manyTill anyChar ((ignore . lookAhead . try . string $ "{{") <|> eof)
    case txt of
        [] -> unexpected "{{"
        _ -> return . LiteralS . unsafeRawHtml . Text.pack $ txt

expressionP :: Monad m => Parser m Expression
expressionP = stringLiteralExprP

stringLiteralExprP :: Monad m => Parser m Expression
stringLiteralExprP = do
    d <- oneOf [ '\'', '\"' ]
    txt <- manyTill stringCharP (char d)
    return . StringLiteralE . Text.pack $ txt

stringCharP :: Monad m => Parser m Char
stringCharP = do
    c1 <- anyChar
    case c1 of
        '\\' -> do
            c2 <- anyChar
            case c2 of
                'n' -> return '\n'
                'b' -> return '\b'
                'v' -> return '\v'
                '0' -> return '\0'
                't' -> return '\t'
                _ -> return c2
        _ -> return c1
