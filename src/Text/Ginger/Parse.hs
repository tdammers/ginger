{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
-- | Ginger parser.
module Text.Ginger.Parse
( parseGinger
, parseGingerFile
, ParserError (..)
, formatParserError
, IncludeResolver
, Source, SourceName
, SourcePos
, sourceLine
, sourceColumn
, sourceName
)
where

import Text.Parsec ( ParseError
                   , SourcePos
                   , SourceName (..)
                   , sourceLine
                   , sourceColumn
                   , sourceName
                   , ParsecT
                   , runParserT
                   , try, lookAhead
                   , manyTill, oneOf, string, notFollowedBy, between, sepBy
                   , eof, spaces, anyChar, noneOf, char
                   , option, optionMaybe
                   , unexpected
                   , digit
                   , getState, modifyState, putState
                   , (<?>)
                   , getPosition
                   )
import Text.Parsec.Error ( errorMessages
                         , errorPos
                         , showErrorMessages
                         )
import Text.Ginger.AST
import Text.Ginger.Html ( unsafeRawHtml )
import Text.Ginger.GVal (GVal, ToGVal (..), dict, (~>))

import Control.Monad (when)
import Control.Monad.Reader ( ReaderT
                            , runReaderT
                            , ask, asks
                            )
import Control.Monad.Trans.Class ( lift )
import Control.Applicative
import Control.Exception (Exception)
import GHC.Generics
import Safe ( readMay )

import Data.Text (Text)
import Data.Maybe ( fromMaybe, catMaybes, listToMaybe )
import Data.Scientific ( Scientific )
import qualified Data.Text as Text
import Data.List ( foldr, nub, sort )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Default ( Default (..) )
import Data.Monoid ( (<>) )
import Data.Char (isSpace)

import System.FilePath ( takeDirectory, (</>) )
import Text.Printf ( printf )

-- | Input type for the parser (source code).
type Source = String

-- | Used to resolve includes. Ginger will call this function whenever it
-- encounters an {% include %}, {% import %}, or {% extends %} directive.
-- If the required source code is not available, the resolver should return
-- @Nothing@, else @Just@ the source.
type IncludeResolver m = SourceName -> m (Maybe Source)

instance ToGVal m SourcePos where
    toGVal p =
        dict [ "name" ~> sourceName p
             , "line" ~> sourceLine p
             , "column" ~> sourceColumn p
             ]

-- | Error information for Ginger parser errors.
data ParserError =
    ParserError
        { peErrorMessage :: String -- ^ Human-readable error message
        , peSourcePosition :: Maybe SourcePos
        }
        deriving (Show, Generic)

instance Exception ParserError where

-- | 
formatParserError :: Maybe String -> ParserError -> String
formatParserError tplSrc e =
    let sourceLocation = do
            pos <- peSourcePosition e
            return $ printf "%s:%i:%i\n"
                (sourceName pos)
                (sourceLine pos)
                (sourceColumn pos)
        markerLines = do
            sourceLines <- lines <$> tplSrc
            pos <- peSourcePosition e
            let lineNum = sourceLine pos
            offendingLine <- listToMaybe . drop (pred lineNum) $ sourceLines
            let offendingColumn = sourceColumn pos
            return . unlines $
                [ offendingLine
                , (replicate (pred offendingColumn) ' ') <> "^"
                ]

    in unlines . catMaybes $
        [ sourceLocation
        , markerLines
        , Just (peErrorMessage e)
        ]

-- | Helper function to create a Ginger parser error from a Parsec error.
fromParsecError :: ParseError -> ParserError
fromParsecError e =
    ParserError
        (dropWhile (== '\n') .
            showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            $ errorMessages e)
        (Just $ errorPos e)

-- | Parse Ginger source from a file.
parseGingerFile :: forall m. Monad m => IncludeResolver m -> SourceName -> m (Either ParserError (Template SourcePos))
parseGingerFile resolver sourceName =
    parseGingerFile' opts sourceName
    where
        opts :: ParserOptions m
        opts =
            (mkParserOptions resolver)
                { poSourceName = Just sourceName }

-- | Parse Ginger source from memory.
parseGinger :: forall m. Monad m => IncludeResolver m -> Maybe SourceName -> Source -> m (Either ParserError (Template SourcePos))
parseGinger resolver sourceName source =
    parseGinger' opts source
    where
        opts :: ParserOptions m
        opts =
            (mkParserOptions resolver)
                { poSourceName = sourceName }

-- | Parse Ginger source from a file.
parseGingerFile' :: Monad m => ParserOptions m -> SourceName -> m (Either ParserError (Template SourcePos))
parseGingerFile' opts' fn = do
    let opts = opts' { poSourceName = Just fn }
    let resolve = poIncludeResolver opts
    srcMay <- resolve fn
    case srcMay of
        Nothing -> return . Left $
            ParserError
                { peErrorMessage = "Template source not found: " ++ fn
                , peSourcePosition = Nothing
                }
        Just src -> parseGinger' opts src

-- | Parse Ginger source from memory.
parseGinger' :: Monad m => ParserOptions m -> Source -> m (Either ParserError (Template SourcePos))
parseGinger' opts src = do
    result <-
        runReaderT
            ( runParserT
                (templateP `before` eof)
                defParseState
                (fromMaybe "<<unknown>>" $ poSourceName opts)
                src
            )
            opts
    case result of
        Right t -> return . Right $ t
        Left e -> return . Left $ fromParsecError e


data ParserOptions m
    = ParserOptions
        { poIncludeResolver :: IncludeResolver m
        , poSourceName :: Maybe SourceName
        }

mkParserOptions :: Monad m => IncludeResolver m -> ParserOptions m
mkParserOptions resolver =
    ParserOptions
        { poIncludeResolver = resolver
        , poSourceName = Nothing
        }

data ParseState
    = ParseState
        { psBlocks :: HashMap VarName (Block SourcePos)
        , psStripIndent :: String
        }

defParseState :: ParseState
defParseState =
    ParseState
        { psBlocks = HashMap.empty
        , psStripIndent = ""
        }

type Parser m a = ParsecT String ParseState (ReaderT (ParserOptions m) m) a

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

getResolver :: Monad m => Parser m (IncludeResolver m)
getResolver = asks poIncludeResolver

include :: Monad m => SourceName -> Parser m (Statement SourcePos)
include sourceName =
  PreprocessedIncludeS
    <$> getPosition
    <*> includeTemplate sourceName

-- include sourceName = templateBody <$> includeTemplate sourceName

includeTemplate :: Monad m => SourceName -> Parser m (Template SourcePos)
includeTemplate sourceName = do
    resolver <- getResolver
    currentSource <- fromMaybe "" <$> asks poSourceName
    let includeSourceName = takeDirectory currentSource </> sourceName
    pres <- lift . lift $ parseGingerFile resolver includeSourceName
    case pres of
        Right t -> return t
        Left err -> fail (show err)

reduceStatements :: SourcePos -> [(Statement SourcePos)] -> (Statement SourcePos)
reduceStatements pos [] = NullS pos
reduceStatements pos [x] = x
reduceStatements pos xs = MultiS pos xs

templateP :: Monad m => Parser m (Template SourcePos)
templateP = derivedTemplateP <|> baseTemplateP

derivedTemplateP :: Monad m => Parser m (Template SourcePos)
derivedTemplateP = do
    pos <- getPosition
    parentName <- try (spaces >> fancyTagP "extends" stringLiteralP)
    parentTemplate <- includeTemplate parentName
    topLevelBlocks <- HashMap.fromList <$> many blockP
    nestedBlocks <- psBlocks <$> getState
    let blocks = topLevelBlocks <> nestedBlocks
    return
        Template
            { templateBody = NullS pos
            , templateParent = Just parentTemplate
            , templateBlocks = blocks
            }

baseTemplateP :: Monad m => Parser m (Template SourcePos)
baseTemplateP = do
    body <- statementsP
    blocks <- psBlocks <$> getState
    return
        Template
            { templateBody = body
            , templateParent = Nothing
            , templateBlocks = blocks
            }

isNullS (NullS _) = True
isNullS _ = False

statementsP :: Monad m => Parser m (Statement SourcePos)
statementsP = do
    pos <- getPosition
    reduceStatements pos . filter (not . isNullS) <$> many (try statementP)

scriptStatementsP :: Monad m => Parser m (Statement SourcePos)
scriptStatementsP = do
    spacesOrComment
    pos <- getPosition
    reduceStatements pos . filter (not . isNullS) <$>
        many (try scriptStatementP)


scriptStatementBlockP :: Monad m => Parser m (Statement SourcePos)
scriptStatementBlockP = do
    char '{'
    spacesOrComment
    inner <- scriptStatementsP
    char '}'
    spacesOrComment
    return inner

statementP :: Monad m => Parser m (Statement SourcePos)
statementP = interpolationStmtP
           <|> commentStmtP
           <|> tryCatchStmtP
           <|> ifStmtP
           <|> switchStmtP
           <|> setStmtP
           <|> forStmtP
           <|> includeP
           <|> macroStmtP
           <|> blockStmtP
           <|> callStmtP
           <|> scopeStmtP
           <|> indentStmtP
           <|> scriptStmtP
           <|> literalStmtP

scriptStatementP :: Monad m => Parser m (Statement SourcePos)
scriptStatementP = scriptStatementBlockP
                 <|> scriptEchoStmtP
                 <|> scriptIfStmtP
                 <|> scriptSwitchStmtP
                 <|> scriptSetStmtP
                 <|> scriptForStmtP
                 <|> scriptIncludeP
                 <|> scriptMacroStmtP
                 <|> scriptScopeStmtP
                 <|> scriptExprStmtP

interpolationStmtP :: Monad m => Parser m (Statement SourcePos)
interpolationStmtP = do
    pos <- getPosition
    try openInterpolationP
    spacesOrComment
    expr <- expressionP
    spacesOrComment
    closeInterpolationP
    return $ InterpolationS pos expr

scriptEchoStmtP :: Monad m => Parser m (Statement SourcePos)
scriptEchoStmtP = do
    pos <- getPosition
    try $ keyword "echo"
    spacesOrComment
    char '('
    expr <- expressionP
    spacesOrComment
    char ')'
    spacesOrComment
    char ';'
    spacesOrComment
    return $ InterpolationS pos expr

literalStmtP :: Monad m => Parser m (Statement SourcePos)
literalStmtP = do
    pos <- getPosition
    txt <- manyTill literalCharP endOfLiteralP

    case txt of
        [] -> unexpected "{{"
        _ -> return . LiteralS pos . unsafeRawHtml . Text.pack $ txt

literalCharP :: Monad m => Parser m Char
literalCharP =
    literalNewlineP <|> anyChar

literalNewlineP :: Monad m => Parser m Char
literalNewlineP = do
    stripStr <- psStripIndent <$> getState
    char '\n'
    when (not $ null stripStr) (ignore . optional . try $ string stripStr)
    return '\n'

endOfLiteralP :: Monad m => Parser m ()
endOfLiteralP =
    (ignore . lookAhead . try $ openInterpolationP) <|>
    (ignore . lookAhead $ openTagP) <|>
    (ignore . lookAhead $ openCommentP) <|>
    eof

commentStmtP :: Monad m => Parser m (Statement SourcePos)
commentStmtP = do
    pos <- getPosition
    try openCommentP
    manyTill
        (   (noneOf "#" *> return ())
        <|> (try $ char '#' *> notFollowedBy (char '}'))
        )
        (try closeCommentP)
    return $ NullS pos

scriptCommentP :: Monad m => Parser m ()
scriptCommentP = do
    try $ char '#' *> notFollowedBy (char '}')
    manyTill anyChar endl
    spacesOrComment

spacesOrComment :: Monad m => Parser m ()
spacesOrComment = do
    many $ scriptCommentP <|> (oneOf " \t\r\n" *> return ())
    return ()

scriptExprStmtP :: Monad m => Parser m (Statement SourcePos)
scriptExprStmtP = do
    pos <- getPosition
    expr <- try $ expressionP
    char ';'
    spacesOrComment
    return $ ExpressionS pos expr

endl :: Monad m => Parser m Char
endl = char '\n' <|> (char '\r' >> char '\n')

scriptStmtP :: Monad m => Parser m (Statement SourcePos)
scriptStmtP =
    between
        (try $ simpleTagP "script")
        (simpleTagP "endscript")
        scriptStatementsP

ifStmtP :: Monad m => Parser m (Statement SourcePos)
ifStmtP = do
    pos <- getPosition
    condExpr <- fancyTagP "if" expressionP
    trueStmt <- statementsP
    falseStmt <- elifBranchP <|> elseBranchP <|> (NullS <$> getPosition)
    simpleTagP "endif"
    return $ IfS pos condExpr trueStmt falseStmt

elseBranchP :: Monad m => Parser m (Statement SourcePos)
elseBranchP = do
    try $ simpleTagP "else"
    statementsP

elifBranchP :: Monad m => Parser m (Statement SourcePos)
elifBranchP = do
    pos <- getPosition
    condExpr <- try $ fancyTagP "elif" expressionP
    trueStmt <- statementsP
    falseStmt <- elifBranchP <|> elseBranchP <|> (NullS <$> getPosition)
    -- No endif here: the parent {% if %} owns that one.
    return $ IfS pos condExpr trueStmt falseStmt

scriptIfStmtP :: Monad m => Parser m (Statement SourcePos)
scriptIfStmtP = do
    pos <- getPosition
    try $ keyword "if"
    spacesOrComment
    char '('
    condExpr <- expressionP
    spacesOrComment
    char ')'
    spacesOrComment
    trueStmt <- scriptStatementP
    spacesOrComment
    falseStmt <- scriptElifP <|> scriptElseP <|> (NullS <$> getPosition)
    return $ IfS pos condExpr trueStmt falseStmt

scriptElseP :: Monad m => Parser m (Statement SourcePos)
scriptElseP = do
    try $ keyword "else"
    spacesOrComment
    scriptStatementP

scriptElifP :: Monad m => Parser m (Statement SourcePos)
scriptElifP = do
    pos <- getPosition
    try $ keyword "elif"
    spacesOrComment
    char '('
    spacesOrComment
    condExpr <- expressionP
    spacesOrComment
    char ')'
    spacesOrComment
    trueStmt <- scriptStatementP
    spacesOrComment
    falseStmt <- scriptElifP <|> scriptElseP <|> (NullS <$> getPosition)
    return $ IfS pos condExpr trueStmt falseStmt

tryCatchStmtP :: Monad m => Parser m (Statement SourcePos)
tryCatchStmtP = do
    pos <- getPosition
    try $ simpleTagP "try"
    tryS <- statementsP
    catchesS <- many catchBranchP
    finallyS <- finallyBranchP <|> (NullS <$> getPosition)
    simpleTagP "endtry"
    return $ TryCatchS pos tryS catchesS finallyS

catchBranchP :: Monad m => Parser m (CatchBlock SourcePos)
catchBranchP = do
    (what, captureName) <- try $
        fancyTagP "catch" (try catchHeaderP <|> return (Nothing, Nothing))
    body <- statementsP
    return $ Catch what captureName body

suchThat :: Monad m => (a -> Bool) -> Parser m a -> Parser m a
suchThat p action = do
    val <- action
    if p val then return val else fail "Requirement not met"

catchHeaderP :: Monad m => Parser m (Maybe Text, Maybe VarName)
catchHeaderP = do
    spaces
    what <- catchWhatP
    spaces
    captureName <- catchCaptureP
    return $ (what, captureName)

catchWhatP :: Monad m => Parser m (Maybe Text)
catchWhatP =
    (Nothing <$ char '*') <|>
    (Just . Text.pack <$> try stringLiteralP) <|>
    (Just <$> try identifierP)

catchCaptureP :: Monad m => Parser m (Maybe VarName)
catchCaptureP = optionMaybe $ do
    try (string "as" >> notFollowedBy identCharP)
    spaces
    identifierP

finallyBranchP :: Monad m => Parser m (Statement SourcePos)
finallyBranchP = do
    try $ simpleTagP "finally"
    statementsP

-- TODO: try/catch/finally in script mode

switchStmtP :: Monad m => Parser m (Statement SourcePos)
switchStmtP = do
    pos <- getPosition
    pivotExpr <- try $ fancyTagP "switch" expressionP
    cases <- many switchCaseP
    def <- switchDefaultP <|> (NullS <$> getPosition)
    simpleTagP "endswitch"
    return $ SwitchS pos pivotExpr cases def

switchCaseP :: Monad m => Parser m ((Expression SourcePos), (Statement SourcePos))
switchCaseP = do
    cmpExpr <- try $ fancyTagP "case" expressionP
    body <- statementsP
    simpleTagP "endcase"
    return (cmpExpr, body)

switchDefaultP :: Monad m => Parser m (Statement SourcePos)
switchDefaultP = do
    try (simpleTagP "default") *> statementsP <* simpleTagP "enddefault"

scriptSwitchStmtP :: Monad m => Parser m (Statement SourcePos)
scriptSwitchStmtP = do
    pos <- getPosition
    try $ keyword "switch"
    spacesOrComment
    char '('
    spacesOrComment
    pivotExpr <- expressionP
    spacesOrComment
    char ')'
    spacesOrComment
    char '{'
    spacesOrComment
    cases <- many scriptSwitchCaseP
    def <- scriptSwitchDefaultP <|> (NullS <$> getPosition)
    spacesOrComment
    char '}'
    spacesOrComment
    return $ SwitchS pos pivotExpr cases def

scriptSwitchCaseP :: Monad m => Parser m ((Expression SourcePos), (Statement SourcePos))
scriptSwitchCaseP = do
    try $ keyword "case"
    spacesOrComment
    cmpExpr <- expressionP
    spacesOrComment
    char ':'
    spacesOrComment
    body <- scriptStatementP
    spacesOrComment
    return (cmpExpr, body)

scriptSwitchDefaultP :: Monad m => Parser m (Statement SourcePos)
scriptSwitchDefaultP = do
    try $ keyword "default"
    spacesOrComment
    char ':'
    spacesOrComment
    body <- scriptStatementP
    spacesOrComment
    return body

setStmtP :: Monad m => Parser m (Statement SourcePos)
setStmtP = do
    pos <- getPosition
    fancyTagP "set" (setStmtInnerP pos)

setStmtInnerP :: Monad m => SourcePos -> Parser m (Statement SourcePos)
setStmtInnerP pos = do
    name <- identifierP
    spacesOrComment
    char '='
    spacesOrComment
    val <- expressionP
    spacesOrComment
    return $ SetVarS pos name val

scriptSetStmtP :: Monad m => Parser m (Statement SourcePos)
scriptSetStmtP = do
    pos <- getPosition
    try $ keyword "set"
    spacesOrComment
    name <- identifierP
    spacesOrComment
    char '='
    spacesOrComment
    val <- expressionP
    spacesOrComment
    char ';'
    spacesOrComment
    return $ SetVarS pos name val

defineBlock :: VarName -> Block SourcePos -> ParseState -> ParseState
defineBlock name block s =
    s { psBlocks = HashMap.insert name block (psBlocks s) }

blockStmtP :: Monad m => Parser m (Statement SourcePos)
blockStmtP = do
    pos <- getPosition
    (name, block) <- blockP
    modifyState (defineBlock name block)
    return $ BlockRefS pos name

blockP :: Monad m => Parser m (VarName, (Block SourcePos))
blockP = do
    name <- fancyTagP "block" identifierP
    body <- statementsP
    fancyTagP "endblock" (optional $ string (Text.unpack name) >> spacesOrComment)
    return (name, Block body)

macroStmtP :: Monad m => Parser m (Statement SourcePos)
macroStmtP = do
    pos <- getPosition
    (name, args) <- try $ fancyTagP "macro" macroHeadP
    body <- statementsP
    fancyTagP "endmacro" (optional $ string (Text.unpack name) >> spacesOrComment)
    return $ DefMacroS pos name (Macro args body)

scriptMacroStmtP :: Monad m => Parser m (Statement SourcePos)
scriptMacroStmtP = do
    pos <- getPosition
    try $ keyword "macro"
    spacesOrComment
    name <- identifierP
    spacesOrComment
    args <- option [] $ groupP "(" ")" identifierP
    spacesOrComment
    body <- scriptStatementP
    spacesOrComment
    return $ DefMacroS pos name (Macro args body)

macroHeadP :: Monad m => Parser m (VarName, [VarName])
macroHeadP = do
    name <- identifierP
    spacesOrComment
    args <- option [] $ groupP "(" ")" identifierP
    spacesOrComment
    return (name, args)

-- {% call (foo) bar(baz) %}quux{% endcall %}
--
-- is the same as:
--
-- {% scope %}
-- {% macro __lambda(foo) %}quux{% endmacro %}
-- {% set caller = __lambda %}
-- {{ bar(baz) }}
-- {% endscope %]
callStmtP :: Monad m => Parser m (Statement SourcePos)
callStmtP = do
    pos <- getPosition
    (callerArgs, call) <- try $ fancyTagP "call" callHeadP
    body <- statementsP
    simpleTagP "endcall"
    return (
        ScopedS pos (
            MultiS pos
                [ DefMacroS pos "caller" (Macro callerArgs body)
                , InterpolationS pos call
                ]))

callHeadP :: Monad m => Parser m ([Text], (Expression SourcePos))
callHeadP = do
    callerArgs <- option [] $ groupP "(" ")" identifierP
    spacesOrComment
    call <- expressionP
    spacesOrComment
    return (callerArgs, call)

scopeStmtP :: Monad m => Parser m (Statement SourcePos)
scopeStmtP =
    ScopedS
        <$> getPosition
        <*> between
            (try $ simpleTagP "scope")
            (simpleTagP "endscope")
            statementsP

indentStmtP :: Monad m => Parser m (Statement SourcePos)
indentStmtP = do
    pos <- getPosition
    indentExpr <- try $ fancyTagP "indent" indentHeadP
    preIndent <- many (oneOf " \t")
    oldState <- getState
    modifyState $ \state ->
        state { psStripIndent = preIndent }
    body <- statementsP
    putState oldState
    simpleTagP "endindent"
    return $ IndentS pos indentExpr body

indentHeadP :: Monad m => Parser m (Expression SourcePos)
indentHeadP =
    (expressionP <|> (StringLiteralE <$> getPosition <*> pure "  ")) <* spacesOrComment

scriptScopeStmtP :: Monad m => Parser m (Statement SourcePos)
scriptScopeStmtP = do
    pos <- getPosition
    try $ keyword "scope"
    spacesOrComment
    ScopedS pos <$> scriptStatementP

forStmtP :: Monad m => Parser m (Statement SourcePos)
forStmtP = do
    pos <- getPosition
    (iteree, varNameVal, varNameIndex) <- fancyTagP "for" forHeadP
    body <- statementsP
    elseBranchMay <- optionMaybe $ do
        try $ simpleTagP "else"
        statementsP
    simpleTagP "endfor"
    let forLoop = ForS pos varNameIndex varNameVal iteree body
    return $ maybe
        forLoop
        (IfS pos iteree forLoop)
        elseBranchMay

scriptForStmtP :: Monad m => Parser m (Statement SourcePos)
scriptForStmtP = do
    pos <- getPosition
    try $ keyword "for"
    spacesOrComment
    char '('
    (iteree, varNameVal, varNameIndex) <- forHeadP
    spacesOrComment
    char ')'
    spacesOrComment
    body <- scriptStatementP
    elseBranchMay <- optionMaybe $ do
        try $ keyword "else"
        spacesOrComment
        scriptStatementP
    let forLoop = ForS pos varNameIndex varNameVal iteree body
    return $ maybe
        forLoop
        (IfS pos iteree forLoop)
        elseBranchMay

includeP :: Monad m => Parser m (Statement SourcePos)
includeP = do
    sourceName <- fancyTagP "include" stringLiteralP
    include sourceName

scriptIncludeP :: Monad m => Parser m (Statement SourcePos)
scriptIncludeP = do
    try $ keyword "include"
    spacesOrComment
    char '('
    sourceName <- stringLiteralP
    spacesOrComment
    char ')'
    spacesOrComment
    char ';'
    spacesOrComment
    include sourceName

forHeadP :: Monad m => Parser m ((Expression SourcePos), VarName, Maybe VarName)
forHeadP =
    (try forHeadInP <|> forHeadAsP) <* optional (keyword "recursive" >>spacesOrComment)

forIteratorP :: Monad m => Parser m (VarName, Maybe VarName)
forIteratorP = try forIndexedIteratorP <|> try forSimpleIteratorP <?> "iteration variables"

forIndexedIteratorP :: Monad m => Parser m (VarName, Maybe VarName)
forIndexedIteratorP = do
    indexIdent <- identifierP
    spacesOrComment
    char ','
    spacesOrComment
    varIdent <- identifierP
    spacesOrComment
    return (varIdent, Just indexIdent)

forSimpleIteratorP :: Monad m => Parser m (VarName, Maybe VarName)
forSimpleIteratorP = do
    varIdent <- identifierP
    spacesOrComment
    return (varIdent, Nothing)

forHeadInP :: Monad m => Parser m ((Expression SourcePos), VarName, Maybe VarName)
forHeadInP = do
    (varIdent, indexIdent) <- forIteratorP
    spacesOrComment
    keyword "in"
    spacesOrComment
    iteree <- expressionP
    return (iteree, varIdent, indexIdent)

forHeadAsP :: Monad m => Parser m ((Expression SourcePos), VarName, Maybe VarName)
forHeadAsP = do
    iteree <- expressionP
    spacesOrComment
    keyword "as"
    spacesOrComment
    (varIdent, indexIdent) <- forIteratorP
    return (iteree, varIdent, indexIdent)

fancyTagP :: Monad m => String -> Parser m a -> Parser m a
fancyTagP tagName =
    between
        (try $ do
            openTagP
            keyword tagName
            spacesOrComment)
        closeTagP

simpleTagP :: Monad m => String -> Parser m ()
simpleTagP tagName = openTagP >> string tagName >> closeTagP

openInterpolationP :: Monad m => Parser m ()
openInterpolationP = openP '{'

closeInterpolationP :: Monad m => Parser m ()
closeInterpolationP = closeP '}'

openCommentP :: Monad m => Parser m ()
openCommentP = openP '#'

closeCommentP :: Monad m => Parser m ()
closeCommentP = closeP '#'

openTagP :: Monad m => Parser m ()
openTagP = openP '%'

closeTagP :: Monad m => Parser m ()
closeTagP = do
    closeP '%'
    ignore . optional $ literalNewlineP

openP :: Monad m => Char -> Parser m ()
openP c = try (openWP c) <|> try (openNWP c)

openWP :: Monad m => Char -> Parser m ()
openWP c = ignore $ do
    spaces
    string [ '{', c, '-' ]
    spacesOrComment

openNWP :: Monad m => Char -> Parser m ()
openNWP c = ignore $ do
    string [ '{', c ]
    spacesOrComment

closeP :: Monad m => Char -> Parser m ()
closeP c = try (closeWP c) <|> try (closeNWP c)

closeWP :: Monad m => Char -> Parser m ()
closeWP c = ignore $ do
    spacesOrComment
    string [ '-', c, '}' ]
    spaces

closeNWP :: Monad m => Char -> Parser m ()
closeNWP c = ignore $ do
    spacesOrComment
    string [ c, '}' ]

expressionP :: Monad m => Parser m (Expression SourcePos)
expressionP = lambdaExprP <|> ternaryExprP

lambdaExprP :: Monad m => Parser m (Expression SourcePos)
lambdaExprP = do
    pos <- getPosition
    argNames <- try $ do
        char '('
        spacesOrComment
        argNames <- sepBy (spacesOrComment>> identifierP) (try $ spacesOrComment>> char ',')
        char ')'
        spacesOrComment
        string "->"
        spacesOrComment
        return argNames
    body <- expressionP
    return $ LambdaE pos argNames body

operativeExprP :: forall m. Monad m => Parser m (Expression SourcePos) -> [ (String, Text) ] -> Parser m (Expression SourcePos)
operativeExprP operandP operators = do
    pos0 <- getPosition
    lhs <- operandP
    spacesOrComment
    tails <- many . try $ operativeTail pos0
    return $ foldl (flip ($)) lhs tails
    where
        opChars :: [Char]
        opChars = nub . sort . concatMap fst $ operators
        operativeTail :: SourcePos -> Parser m (Expression SourcePos -> Expression SourcePos)
        operativeTail pos0 = do
            pos <- getPosition
            funcName <-
                foldl (<|>) (fail "operator")
                    [ try (string op >> notFollowedBy (oneOf opChars)) >> return fn | (op, fn) <- operators ]
            spacesOrComment
            rhs <- operandP
            spacesOrComment
            return (\lhs -> CallE pos0 (VarE pos funcName) [(Nothing, lhs), (Nothing, rhs)])

ternaryExprP :: Monad m => Parser m (Expression SourcePos)
ternaryExprP = do
    pos <- getPosition
    expr1 <- booleanExprP
    spacesOrComment
    cTernaryTailP pos expr1 <|> pyTernaryTailP pos expr1 <|> return expr1

cTernaryTailP :: Monad m => SourcePos -> (Expression SourcePos) -> Parser m (Expression SourcePos)
cTernaryTailP pos condition = do
    char '?'
    spacesOrComment
    yesBranch <- expressionP
    char ':'
    spacesOrComment
    noBranch <- expressionP
    return $ TernaryE pos condition yesBranch noBranch

pyTernaryTailP :: Monad m => SourcePos -> (Expression SourcePos) -> Parser m (Expression SourcePos)
pyTernaryTailP pos yesBranch = do
    keyword "if"
    spacesOrComment
    condition <- booleanExprP
    keyword "else"
    spacesOrComment
    noBranch <- expressionP
    return $ TernaryE pos condition yesBranch noBranch

booleanExprP :: Monad m => Parser m (Expression SourcePos)
booleanExprP =
    operativeExprP
        comparativeExprP
        [ ("||", "any")
        , ("&&", "all")
        ]

comparativeExprP :: Monad m => Parser m (Expression SourcePos)
comparativeExprP =
    operativeExprP
        additiveExprP
        [ ("==", "equals")
        , ("!=", "nequals")
        , (">=", "greaterEquals")
        , ("<=", "lessEquals")
        , (">", "greater")
        , ("<", "less")
        ]

additiveExprP :: Monad m => Parser m (Expression SourcePos)
additiveExprP =
    operativeExprP
        multiplicativeExprP
        [ ("+", "sum")
        , ("-", "difference")
        , ("~", "concat")
        ]

multiplicativeExprP :: Monad m => Parser m (Expression SourcePos)
multiplicativeExprP =
    operativeExprP
        postfixExprP
        [ ("*", "product")
        , ("//", "int_ratio")
        , ("/", "ratio")
        , ("%", "modulo")
        ]

postfixExprP :: Monad m => Parser m (Expression SourcePos)
postfixExprP = do
    pos <- getPosition
    base <- atomicExprP
    spacesOrComment
    postfixes <- many . try $ postfixP pos `before`spacesOrComment
    return $ foldl (flip ($)) base postfixes

postfixP :: Monad m => SourcePos -> Parser m ((Expression SourcePos) -> (Expression SourcePos))
postfixP pos = dotPostfixP pos
             <|> arrayAccessP
             <|> funcCallP
             <|> filterP

dotPostfixP :: Monad m => SourcePos -> Parser m ((Expression SourcePos) -> (Expression SourcePos))
dotPostfixP pos = do
    char '.'
    spacesOrComment
    i <- StringLiteralE <$> getPosition <*> identifierP
    return $ \e -> MemberLookupE pos e i

arrayAccessP :: Monad m => Parser m ((Expression SourcePos) -> (Expression SourcePos))
arrayAccessP = do
    pos <- getPosition
    bracedP "[" "]" (inner pos)
    where
        inner pos = try (sliceInner pos) <|> indexInner pos
        sliceInner pos = do
            offset <- try expressionP <|> (NullLiteralE <$> getPosition)
            char ':'
            length <- try expressionP <|> (NullLiteralE <$> getPosition)
            return $ \e ->
                CallE
                    pos
                    (VarE pos "slice")
                    [ (Nothing, e)
                    , (Nothing, offset)
                    , (Nothing, length)
                    ]
        indexInner pos = do
            i <- expressionP
            return $ \e -> MemberLookupE pos e i

funcCallP :: Monad m => Parser m ((Expression SourcePos) -> (Expression SourcePos))
funcCallP = do
    pos <- getPosition
    args <- groupP "(" ")" funcArgP
    return $ \e -> CallE pos e args

funcArgP :: Monad m => Parser m (Maybe Text, (Expression SourcePos))
funcArgP = namedFuncArgP <|> positionalFuncArgP

namedFuncArgP :: Monad m => Parser m (Maybe Text, (Expression SourcePos))
namedFuncArgP = do
    name <- try $ identifierP `before` between spacesOrComment spacesOrComment (string "=")
    expr <- expressionP
    return (Just name, expr)

positionalFuncArgP :: Monad m => Parser m (Maybe Text, (Expression SourcePos))
positionalFuncArgP = try $ (Nothing,) <$> expressionP

filterP :: Monad m => Parser m ((Expression SourcePos) -> (Expression SourcePos))
filterP = do
    pos <- getPosition
    char '|'
    spacesOrComment
    func <- atomicExprP
    args <- option [] $ groupP "(" ")" funcArgP
    return $ \e -> CallE pos func ((Nothing, e):args)

atomicExprP :: Monad m => Parser m (Expression SourcePos)
atomicExprP = doExprP
            <|> parenthesizedExprP
            <|> objectExprP
            <|> listExprP
            <|> stringLiteralExprP
            <|> numberLiteralExprP
            <|> varExprP

parenthesizedExprP :: Monad m => Parser m (Expression SourcePos)
parenthesizedExprP =
    between
        (try . ignore $ char '(' >> spacesOrComment)
        (ignore $ char ')' >> spacesOrComment)
        expressionP

doExprP :: Monad m => Parser m (Expression SourcePos)
doExprP = do
    pos <- getPosition
    try $ keyword "do"
    spacesOrComment
    stmt <- scriptStatementP
    spacesOrComment
    return $ DoE pos stmt

listExprP :: Monad m => Parser m (Expression SourcePos)
listExprP =
    ListE
        <$> getPosition
        <*> groupP "[" "]" expressionP

objectExprP :: Monad m => Parser m (Expression SourcePos)
objectExprP = ObjectE
    <$> getPosition
    <*> groupP "{" "}" expressionPairP

expressionPairP :: Monad m => Parser m ((Expression SourcePos), (Expression SourcePos))
expressionPairP = do
    a <- expressionP
    spacesOrComment
    char ':'
    spacesOrComment
    b <- expressionP
    spacesOrComment
    return (a, b)

groupP :: Monad m => String -> String -> Parser m a -> Parser m [a]
groupP obr cbr inner =
    bracedP obr cbr
        (sepBy (inner `before` spacesOrComment) (try $ string "," `before` spacesOrComment))

bracedP :: Monad m => String -> String -> Parser m a -> Parser m a
bracedP obr cbr =
    between
        (try . ignore $ string obr >> spacesOrComment)
        (ignore $ string cbr >> spacesOrComment)

varExprP :: Monad m => Parser m (Expression SourcePos)
varExprP = do
    pos <- getPosition
    litName <- identifierP
    spacesOrComment
    return $ case litName of
        "true" -> BoolLiteralE pos True
        "false" -> BoolLiteralE pos False
        "null" -> NullLiteralE pos
        _ -> VarE pos litName

identifierP :: Monad m => Parser m Text
identifierP =
    Text.pack <$> (
    (:)
        <$> oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
        <*> many identCharP)

identCharP :: Monad m => Parser m Char
identCharP = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'])

stringLiteralExprP :: Monad m => Parser m (Expression SourcePos)
stringLiteralExprP =
    StringLiteralE
      <$> getPosition
      <*> (Text.pack <$> stringLiteralP)

stringLiteralP :: Monad m => Parser m String
stringLiteralP = do
    d <- oneOf [ '\'', '\"' ]
    manyTill stringCharP (char d)

stringCharP :: Monad m => Parser m Char
stringCharP = do
    c1 <- anyChar
    case c1 of
        '\\' -> do
            c2 <- anyChar
            case c2 of
                'n' -> return '\n'
                'r' -> return '\r'
                'b' -> return '\b'
                'v' -> return '\v'
                '0' -> return '\0'
                't' -> return '\t'
                _ -> return c2
        _ -> return c1

numberLiteralExprP :: Monad m => Parser m (Expression SourcePos)
numberLiteralExprP = do
    pos <- getPosition
    str <- numberLiteralP
    let nMay :: Maybe Scientific
        nMay = readMay str
    case nMay of
        Just n -> return . NumberLiteralE pos $ n
        Nothing -> fail $ "Failed to parse " ++ str ++ " as a number"

numberLiteralP :: Monad m => Parser m String
numberLiteralP = do
    sign <- option "" $ string "-"
    integral <- string "0" <|> ((:) <$> oneOf ['1'..'9'] <*> many digit)
    fractional <- option "" $ (:) <$> char '.' <*> many digit
    return $ sign ++ integral ++ fractional

followedBy :: Monad m => m b -> m a -> m a
followedBy b a = a >>= \x -> b >> return x

before :: Monad m => m a -> m b -> m a
before = flip followedBy

keyword :: Monad m => String -> Parser m String
keyword kw = do
    string kw
    notFollowedBy identCharP
    return kw
