{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric #-}
-- | Ginger parser.
module Text.Ginger.Parse
( parseGinger
, parseGingerFile
, ParserError (..)
, formatParserError
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
                   , try, lookAhead
                   , manyTill, oneOf, string, notFollowedBy, between, sepBy
                   , eof, spaces, anyChar, noneOf, char
                   , option, optionMaybe
                   , unexpected
                   , digit
                   , getState, modifyState, putState
                   , (<?>)
                   )
import Text.Parsec.Error ( errorMessages
                         , errorPos
                         , showErrorMessages
                         )
import Text.Ginger.AST
import Text.Ginger.Html ( unsafeRawHtml )

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
        deriving (Show, Generic)

instance Exception ParserError where

-- | 
formatParserError :: Maybe String -> ParserError -> String
formatParserError tplSrc e =
    let sourceLocation = do
            printf "%s:%i:%i\n"
                <$> peSourceName e
                <*> peSourceLine e
                <*> peSourceColumn e
        markerLines = do
            sourceLines <- lines <$> tplSrc
            lineNum <- peSourceLine e
            offendingLine <- listToMaybe . drop (pred lineNum) $ sourceLines
            offendingColumn <- peSourceColumn e <|> Just 1
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
                { peErrorMessage = "Template source not found: " ++ fn
                , peSourceName = Nothing
                , peSourceLine = Nothing
                , peSourceColumn = Nothing
                }
        Just src -> parseGinger resolve (Just fn) src


data ParseContext m
    = ParseContext
        { pcResolve :: IncludeResolver m
        , pcCurrentSource :: Maybe SourceName
        }

data ParseState
    = ParseState
        { psBlocks :: HashMap VarName Block
        , psStripIndent :: String
        }

defParseState :: ParseState
defParseState =
    ParseState
        { psBlocks = HashMap.empty
        , psStripIndent = ""
        }

-- | Parse Ginger source from memory.
parseGinger :: Monad m => IncludeResolver m -> Maybe SourceName -> Source -> m (Either ParserError Template)
parseGinger resolve sn src = do
    result <- runReaderT (runParserT (templateP `before` eof) defParseState (fromMaybe "<<unknown>>" sn) src) (ParseContext resolve sn)
    case result of
        Right t -> return . Right $ t
        Left e -> return . Left $ fromParsecError e

type Parser m a = ParsecT String ParseState (ReaderT (ParseContext m) m) a

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

getResolver :: Monad m => Parser m (IncludeResolver m)
getResolver = asks pcResolve

include :: Monad m => SourceName -> Parser m Statement
include sourceName = PreprocessedIncludeS <$> includeTemplate sourceName

-- include sourceName = templateBody <$> includeTemplate sourceName

includeTemplate :: Monad m => SourceName -> Parser m Template
includeTemplate sourceName = do
    resolver <- getResolver
    currentSource <- fromMaybe "" <$> asks pcCurrentSource
    let includeSourceName = takeDirectory currentSource </> sourceName
    pres <- lift . lift $ parseGingerFile resolver includeSourceName
    case pres of
        Right t -> return t
        Left err -> fail (show err)

reduceStatements :: [Statement] -> Statement
reduceStatements [] = NullS
reduceStatements [x] = x
reduceStatements xs = MultiS xs

templateP :: Monad m => Parser m Template
templateP = derivedTemplateP <|> baseTemplateP

derivedTemplateP :: Monad m => Parser m Template
derivedTemplateP = do
    parentName <- try (spaces >> fancyTagP "extends" stringLiteralP)
    parentTemplate <- includeTemplate parentName
    topLevelBlocks <- HashMap.fromList <$> many blockP
    nestedBlocks <- psBlocks <$> getState
    let blocks = topLevelBlocks <> nestedBlocks
    return Template { templateBody = NullS, templateParent = Just parentTemplate, templateBlocks = blocks }

baseTemplateP :: Monad m => Parser m Template
baseTemplateP = do
    body <- statementsP
    blocks <- psBlocks <$> getState
    return Template { templateBody = body, templateParent = Nothing, templateBlocks = blocks }

isNullS NullS = True
isNullS _ = False

statementsP :: Monad m => Parser m Statement
statementsP =
    reduceStatements . filter (not . isNullS) <$> many (try statementP)

scriptStatementsP :: Monad m => Parser m Statement
scriptStatementsP = do
    spacesOrComment
    reduceStatements . filter (not . isNullS) <$>
        many (try scriptStatementP)


scriptStatementBlockP :: Monad m => Parser m Statement
scriptStatementBlockP = do
    char '{'
    spacesOrComment
    inner <- scriptStatementsP
    char '}'
    spacesOrComment
    return inner

statementP :: Monad m => Parser m Statement
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

scriptStatementP :: Monad m => Parser m Statement
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

interpolationStmtP :: Monad m => Parser m Statement
interpolationStmtP = do
    try openInterpolationP
    spacesOrComment
    expr <- expressionP
    spacesOrComment
    closeInterpolationP
    return $ InterpolationS expr

scriptEchoStmtP :: Monad m => Parser m Statement
scriptEchoStmtP = do
    try $ keyword "echo"
    spacesOrComment
    char '('
    expr <- expressionP
    spacesOrComment
    char ')'
    spacesOrComment
    char ';'
    spacesOrComment
    return $ InterpolationS expr

literalStmtP :: Monad m => Parser m Statement
literalStmtP = do
    txt <- manyTill literalCharP endOfLiteralP

    case txt of
        [] -> unexpected "{{"
        _ -> return . LiteralS . unsafeRawHtml . Text.pack $ txt

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

commentStmtP :: Monad m => Parser m Statement
commentStmtP = do
    try openCommentP
    manyTill
        (   (noneOf "#" *> return ())
        <|> (try $ char '#' *> notFollowedBy (char '}'))
        )
        (try closeCommentP)
    return NullS

scriptCommentP :: Monad m => Parser m ()
scriptCommentP = do
    try $ char '#' *> notFollowedBy (char '}')
    manyTill anyChar endl
    spacesOrComment

spacesOrComment :: Monad m => Parser m ()
spacesOrComment = do
    many $ scriptCommentP <|> (oneOf " \t\r\n" *> return ())
    return ()

scriptExprStmtP :: Monad m => Parser m Statement
scriptExprStmtP = do
    expr <- try $ expressionP
    char ';'
    spacesOrComment
    return $ ExpressionS expr

endl :: Monad m => Parser m Char
endl = char '\n' <|> (char '\r' >> char '\n')

scriptStmtP :: Monad m => Parser m Statement
scriptStmtP =
    between
        (try $ simpleTagP "script")
        (simpleTagP "endscript")
        scriptStatementsP

ifStmtP :: Monad m => Parser m Statement
ifStmtP = do
    condExpr <- fancyTagP "if" expressionP
    trueStmt <- statementsP
    falseStmt <- elifBranchP <|> elseBranchP <|> return NullS
    simpleTagP "endif"
    return $ IfS condExpr trueStmt falseStmt

elseBranchP :: Monad m => Parser m Statement
elseBranchP = do
    try $ simpleTagP "else"
    statementsP

elifBranchP :: Monad m => Parser m Statement
elifBranchP = do
    condExpr <- try $ fancyTagP "elif" expressionP
    trueStmt <- statementsP
    falseStmt <- elifBranchP <|> elseBranchP <|> return NullS
    -- No endif here: the parent {% if %} owns that one.
    return $ IfS condExpr trueStmt falseStmt

scriptIfStmtP :: Monad m => Parser m Statement
scriptIfStmtP = do
    try $ keyword "if"
    spacesOrComment
    char '('
    condExpr <- expressionP
    spacesOrComment
    char ')'
    spacesOrComment
    trueStmt <- scriptStatementP
    spacesOrComment
    falseStmt <- scriptElifP <|> scriptElseP <|> return NullS
    return $ IfS condExpr trueStmt falseStmt

scriptElseP :: Monad m => Parser m Statement
scriptElseP = do
    try $ keyword "else"
    spacesOrComment
    scriptStatementP

scriptElifP :: Monad m => Parser m Statement
scriptElifP = do
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
    falseStmt <- scriptElifP <|> scriptElseP <|> return NullS
    return $ IfS condExpr trueStmt falseStmt

tryCatchStmtP :: Monad m => Parser m Statement
tryCatchStmtP = do
    try $ simpleTagP "try"
    tryS <- statementsP
    catchesS <- many catchBranchP
    finallyS <- finallyBranchP <|> return NullS
    simpleTagP "endtry"
    return $ TryCatchS tryS catchesS finallyS

catchBranchP :: Monad m => Parser m CatchBlock
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

finallyBranchP :: Monad m => Parser m Statement
finallyBranchP = do
    try $ simpleTagP "finally"
    statementsP

-- TODO: try/catch/finally in script mode


switchStmtP :: Monad m => Parser m Statement
switchStmtP = do
    pivotExpr <- try $ fancyTagP "switch" expressionP
    cases <- many switchCaseP
    def <- option NullS $ switchDefaultP
    simpleTagP "endswitch"
    return $ SwitchS pivotExpr cases def

switchCaseP :: Monad m => Parser m (Expression, Statement)
switchCaseP = do
    cmpExpr <- try $ fancyTagP "case" expressionP
    body <- statementsP
    simpleTagP "endcase"
    return (cmpExpr, body)

switchDefaultP :: Monad m => Parser m Statement
switchDefaultP = do
    try (simpleTagP "default") *> statementsP <* simpleTagP "enddefault"

scriptSwitchStmtP :: Monad m => Parser m Statement
scriptSwitchStmtP = do
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
    def <- option NullS $ scriptSwitchDefaultP
    spacesOrComment
    char '}'
    spacesOrComment
    return $ SwitchS pivotExpr cases def

scriptSwitchCaseP :: Monad m => Parser m (Expression, Statement)
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

scriptSwitchDefaultP :: Monad m => Parser m Statement
scriptSwitchDefaultP = do
    try $ keyword "default"
    spacesOrComment
    char ':'
    spacesOrComment
    body <- scriptStatementP
    spacesOrComment
    return body

setStmtP :: Monad m => Parser m Statement
setStmtP = fancyTagP "set" setStmtInnerP

setStmtInnerP :: Monad m => Parser m Statement
setStmtInnerP = do
    name <- identifierP
    spacesOrComment
    char '='
    spacesOrComment
    val <- expressionP
    spacesOrComment
    return $ SetVarS name val

scriptSetStmtP :: Monad m => Parser m Statement
scriptSetStmtP = do
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
    return $ SetVarS name val

defineBlock :: VarName -> Block -> ParseState -> ParseState
defineBlock name block s =
    s { psBlocks = HashMap.insert name block (psBlocks s) }

blockStmtP :: Monad m => Parser m Statement
blockStmtP = do
    (name, block) <- blockP
    modifyState (defineBlock name block)
    return $ BlockRefS name

blockP :: Monad m => Parser m (VarName, Block)
blockP = do
    name <- fancyTagP "block" identifierP
    body <- statementsP
    fancyTagP "endblock" (optional $ string (Text.unpack name) >> spacesOrComment)
    return (name, Block body)

macroStmtP :: Monad m => Parser m Statement
macroStmtP = do
    (name, args) <- try $ fancyTagP "macro" macroHeadP
    body <- statementsP
    fancyTagP "endmacro" (optional $ string (Text.unpack name) >> spacesOrComment)
    return $ DefMacroS name (Macro args body)

scriptMacroStmtP :: Monad m => Parser m Statement
scriptMacroStmtP = do
    try $ keyword "macro"
    spacesOrComment
    name <- identifierP
    spacesOrComment
    args <- option [] $ groupP "(" ")" identifierP
    spacesOrComment
    body <- scriptStatementP
    spacesOrComment
    return $ DefMacroS name (Macro args body)

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
callStmtP :: Monad m => Parser m Statement
callStmtP = do
    (callerArgs, call) <- try $ fancyTagP "call" callHeadP
    body <- statementsP
    simpleTagP "endcall"
    return (
        ScopedS (
            MultiS [ DefMacroS "caller" (Macro callerArgs body)
                   , InterpolationS call
                   ]))

callHeadP :: Monad m => Parser m ([Text], Expression)
callHeadP = do
    callerArgs <- option [] $ groupP "(" ")" identifierP
    spacesOrComment
    call <- expressionP
    spacesOrComment
    return (callerArgs, call)

scopeStmtP :: Monad m => Parser m Statement
scopeStmtP =
    ScopedS <$>
        between
            (try $ simpleTagP "scope")
            (simpleTagP "endscope")
            statementsP

indentStmtP :: Monad m => Parser m Statement
indentStmtP = do
    indentExpr <- try $ fancyTagP "indent" indentHeadP
    preIndent <- many (oneOf " \t")
    oldState <- getState
    modifyState $ \state ->
        state { psStripIndent = preIndent }
    body <- statementsP
    putState oldState
    simpleTagP "endindent"
    return $ IndentS indentExpr body

indentHeadP :: Monad m => Parser m Expression
indentHeadP =
    (expressionP <|> return (StringLiteralE "  ")) <* spacesOrComment

scriptScopeStmtP :: Monad m => Parser m Statement
scriptScopeStmtP = do
    try $ keyword "scope"
    spacesOrComment
    ScopedS <$> scriptStatementP

forStmtP :: Monad m => Parser m Statement
forStmtP = do
    (iteree, varNameVal, varNameIndex) <- fancyTagP "for" forHeadP
    body <- statementsP
    elseBranchMay <- optionMaybe $ do
        try $ simpleTagP "else"
        statementsP
    simpleTagP "endfor"
    let forLoop = ForS varNameIndex varNameVal iteree body
    return $ maybe
        forLoop
        (IfS iteree forLoop)
        elseBranchMay

scriptForStmtP :: Monad m => Parser m Statement
scriptForStmtP = do
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
    let forLoop = ForS varNameIndex varNameVal iteree body
    return $ maybe
        forLoop
        (IfS iteree forLoop)
        elseBranchMay

includeP :: Monad m => Parser m Statement
includeP = do
    sourceName <- fancyTagP "include" stringLiteralP
    include sourceName

scriptIncludeP :: Monad m => Parser m Statement
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

forHeadP :: Monad m => Parser m (Expression, VarName, Maybe VarName)
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

forHeadInP :: Monad m => Parser m (Expression, VarName, Maybe VarName)
forHeadInP = do
    (varIdent, indexIdent) <- forIteratorP
    spacesOrComment
    keyword "in"
    spacesOrComment
    iteree <- expressionP
    return (iteree, varIdent, indexIdent)

forHeadAsP :: Monad m => Parser m (Expression, VarName, Maybe VarName)
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
closeTagP = closeP '%'

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
    optional . ignore $ literalNewlineP

expressionP :: Monad m => Parser m Expression
expressionP = lambdaExprP <|> ternaryExprP

lambdaExprP :: Monad m => Parser m Expression
lambdaExprP = do
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
    return $ LambdaE argNames body

operativeExprP :: forall m. Monad m => Parser m Expression -> [ (String, Text) ] -> Parser m Expression
operativeExprP operandP operators = do
    lhs <- operandP
    spacesOrComment
    tails <- many . try $ operativeTail
    return $ foldl (flip ($)) lhs tails
    where
        opChars :: [Char]
        opChars = nub . sort . concatMap fst $ operators
        operativeTail :: Parser m (Expression -> Expression)
        operativeTail = do
            funcName <-
                foldl (<|>) (fail "operator")
                    [ try (string op >> notFollowedBy (oneOf opChars)) >> return fn | (op, fn) <- operators ]
            spacesOrComment
            rhs <- operandP
            spacesOrComment
            return (\lhs -> CallE (VarE funcName) [(Nothing, lhs), (Nothing, rhs)])

ternaryExprP :: Monad m => Parser m Expression
ternaryExprP = do
    expr1 <- booleanExprP
    spacesOrComment
    cTernaryTailP expr1 <|> pyTernaryTailP expr1 <|> return expr1

cTernaryTailP :: Monad m => Expression -> Parser m Expression
cTernaryTailP condition = do
    char '?'
    spacesOrComment
    yesBranch <- expressionP
    char ':'
    spacesOrComment
    noBranch <- expressionP
    return $ TernaryE condition yesBranch noBranch

pyTernaryTailP :: Monad m => Expression -> Parser m Expression
pyTernaryTailP yesBranch = do
    keyword "if"
    spacesOrComment
    condition <- booleanExprP
    keyword "else"
    spacesOrComment
    noBranch <- expressionP
    return $ TernaryE condition yesBranch noBranch

booleanExprP :: Monad m => Parser m Expression
booleanExprP =
    operativeExprP
        comparativeExprP
        [ ("||", "any")
        , ("&&", "all")
        ]

comparativeExprP :: Monad m => Parser m Expression
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

additiveExprP :: Monad m => Parser m Expression
additiveExprP =
    operativeExprP
        multiplicativeExprP
        [ ("+", "sum")
        , ("-", "difference")
        , ("~", "concat")
        ]

multiplicativeExprP :: Monad m => Parser m Expression
multiplicativeExprP =
    operativeExprP
        postfixExprP
        [ ("*", "product")
        , ("//", "int_ratio")
        , ("/", "ratio")
        , ("%", "modulo")
        ]

postfixExprP :: Monad m => Parser m Expression
postfixExprP = do
    base <- atomicExprP
    spacesOrComment
    postfixes <- many . try $ postfixP `before`spacesOrComment
    return $ foldl (flip ($)) base postfixes

postfixP :: Monad m => Parser m (Expression -> Expression)
postfixP = dotPostfixP
         <|> arrayAccessP
         <|> funcCallP
         <|> filterP

dotPostfixP :: Monad m => Parser m (Expression -> Expression)
dotPostfixP = do
    char '.'
    spacesOrComment
    i <- StringLiteralE <$> identifierP
    return $ \e -> MemberLookupE e i

arrayAccessP :: Monad m => Parser m (Expression -> Expression)
arrayAccessP = do
    bracedP "[" "]" inner
    where
        inner = try sliceInner <|> indexInner
        sliceInner = do
            offset <- option NullLiteralE (try expressionP)
            char ':'
            length <- option NullLiteralE (try expressionP)
            return $ \e ->
                CallE
                    (VarE "slice")
                    [ (Nothing, e)
                    , (Nothing, offset)
                    , (Nothing, length)
                    ]
        indexInner = do
            i <- expressionP
            return $ \e -> MemberLookupE e i

funcCallP :: Monad m => Parser m (Expression -> Expression)
funcCallP = do
    args <- groupP "(" ")" funcArgP
    return $ \e -> CallE e args

funcArgP :: Monad m => Parser m (Maybe Text, Expression)
funcArgP = namedFuncArgP <|> positionalFuncArgP

namedFuncArgP :: Monad m => Parser m (Maybe Text, Expression)
namedFuncArgP = do
    name <- try $ identifierP `before` between spacesOrComment spacesOrComment (string "=")
    expr <- expressionP
    return (Just name, expr)

positionalFuncArgP :: Monad m => Parser m (Maybe Text, Expression)
positionalFuncArgP = try $ (Nothing,) <$> expressionP

filterP :: Monad m => Parser m (Expression -> Expression)
filterP = do
    char '|'
    spacesOrComment
    func <- atomicExprP
    args <- option [] $ groupP "(" ")" funcArgP
    return $ \e -> CallE func ((Nothing, e):args)

atomicExprP :: Monad m => Parser m Expression
atomicExprP = doExprP
            <|> parenthesizedExprP
            <|> objectExprP
            <|> listExprP
            <|> stringLiteralExprP
            <|> numberLiteralExprP
            <|> varExprP

parenthesizedExprP :: Monad m => Parser m Expression
parenthesizedExprP =
    between
        (try . ignore $ char '(' >> spacesOrComment)
        (ignore $ char ')' >> spacesOrComment)
        expressionP

doExprP :: Monad m => Parser m Expression
doExprP = do
    try $ keyword "do"
    spacesOrComment
    stmt <- scriptStatementP
    spacesOrComment
    return $ DoE stmt

listExprP :: Monad m => Parser m Expression
listExprP = ListE <$> groupP "[" "]" expressionP

objectExprP :: Monad m => Parser m Expression
objectExprP = ObjectE <$> groupP "{" "}" expressionPairP

expressionPairP :: Monad m => Parser m (Expression, Expression)
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

varExprP :: Monad m => Parser m Expression
varExprP = do
    litName <- identifierP
    spacesOrComment
    return $ case litName of
        "true" -> BoolLiteralE True
        "false" -> BoolLiteralE False
        "null" -> NullLiteralE
        _ -> VarE litName

identifierP :: Monad m => Parser m Text
identifierP =
    Text.pack <$> (
    (:)
        <$> oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
        <*> many identCharP)

identCharP :: Monad m => Parser m Char
identCharP = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'])

stringLiteralExprP :: Monad m => Parser m Expression
stringLiteralExprP =
    StringLiteralE . Text.pack <$> stringLiteralP

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

numberLiteralExprP :: Monad m => Parser m Expression
numberLiteralExprP = do
    str <- numberLiteralP
    let nMay :: Maybe Scientific
        nMay = readMay str
    case nMay of
        Just n -> return . NumberLiteralE $ n
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
