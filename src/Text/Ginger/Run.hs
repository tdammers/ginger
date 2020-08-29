{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
-- | Execute Ginger templates in an arbitrary monad.
--
-- Usage example:
--
-- > render :: Template -> Text -> Text -> Text
-- > render template username imageURL = do
-- >    let contextLookup varName =
-- >            case varName of
-- >                "username" -> toGVal username
-- >                "imageURL" -> toGVal imageURL
-- >                _ -> def -- def for GVal is equivalent to a NULL value
-- >        context = makeContextHtml contextLookup
-- >    in htmlSource $ runGinger context template
module Text.Ginger.Run
(
-- * The \"easy\" interface
-- | Provides a straightforward way of rendering templates monadically
-- as well as purely.
  easyRenderM
, easyRender
, easyContext
-- * The \"direct\" interface
-- | This interface gives more control than the easy interface, at the
-- expense of requiring more yak shaving.
, runGingerT
, runGinger
, makeContext
, makeContextM
, makeContext'
, makeContextM'
, makeContextExM'
, makeContextHtml
, makeContextHtmlM
, makeContextHtmlExM
, makeContextText
, makeContextTextM
, makeContextTextExM
-- * The context type
, GingerContext
-- * The Run monad
, Run, liftRun, liftRun2
-- * Helper functions for interpreting argument lists
, extractArgs, extractArgsT, extractArgsL, extractArgsDefL
-- * Hoisting
, hoistContext
, hoistRun
, hoistNewlines
, hoistRunState
-- * Errors
, RuntimeError (..)
, runtimeErrorWhat
, runtimeErrorWhere
, runtimeErrorMessage
)
where

import Prelude ( (.), ($), (==), (/=)
               , (>), (<), (>=), (<=)
               , (+), (-), (*), (/), div, (**), (^)
               , (||), (&&)
               , (++)
               , Show, show
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , Int, Integer, String
               , fromIntegral, floor, round
               , not
               , show
               , uncurry
               , seq
               , fst, snd
               , maybe
               , Either (..)
               , id
               )
import qualified Prelude
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.List as List
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.GVal
import Text.Ginger.Run.Type
import Text.Ginger.Run.Builtins
import Text.Ginger.Run.FuncUtils
import Text.Ginger.Run.VM
import Text.Printf
import Text.PrintfA
import Text.Ginger.Parse (parseGinger, ParserError)
import Control.Monad.Except (runExceptT, throwError, catchError)

import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific, formatScientific)
import qualified Data.Scientific as Scientific
import Data.Default (def)
import Safe (readMay, lastDef, headMay)
import Network.HTTP.Types (urlEncode)
import Debug.Trace (trace)
import Data.List (lookup, zipWith, unzip)
import Data.Aeson as JSON

defaultScope :: forall m h p
              . ( Monoid h
                , Monad m
                , ToGVal (Run p m h) h
                , ToGVal (Run p m h) p
                )
             => [(Text, GVal (Run p m h))]
defaultScope =
    [ ("raw", fromFunction gfnRawHtml)
    , ("abs", fromFunction . unaryNumericFunc 0 $ Prelude.abs)
    , ("any", fromFunction gfnAny)
    , ("all", fromFunction gfnAll)
    , ("apply", fromFunction gfnApply)
    -- TODO: batch
    , ("capitalize", fromFunction . variadicStringFunc $ mconcat . Prelude.map capitalize)
    , ("ceil", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.ceiling)
    , ("center", fromFunction gfnCenter)
    , ("compose", fromFunction gfnCompose)
    , ("concat", fromFunction gfnConcat)
    , ("contains", fromFunction gfnContains)
    , ("d", fromFunction gfnDefault)
    , ("date", fromFunction gfnDateFormat)
    , ("dateformat", fromFunction gfnDateFormat)
    , ("default", fromFunction gfnDefault)
    , ("dictsort", fromFunction gfnDictsort)
    , ("difference", fromFunction . variadicNumericFunc 0 $ difference)
    , ("divisibleby", fromFunction gfnDivisibleBy)
    , ("e", fromFunction gfnEscape)
    , ("eq", fromFunction gfnEquals)
    , ("equals", fromFunction gfnEquals)
    , ("equalto", fromFunction gfnEquals)
    , ("escape", fromFunction gfnEscape)
    , ("eval", fromFunction gfnEval)
    , ("even", fromFunction gfnEven)
    , ("filesizeformat", fromFunction gfnFileSizeFormat)
    , ("filter", fromFunction gfnFilter)
    , ("floor", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.floor)
    , ("format", fromFunction gfnPrintf)
    , ("ge", fromFunction gfnGreaterEquals)
    , ("gt", fromFunction gfnGreater)
    , ("greater", fromFunction gfnGreater)
    , ("greaterthan", fromFunction gfnGreater)
    , ("greaterEquals", fromFunction gfnGreaterEquals)
    , ("int", fromFunction . unaryFunc $ toGVal . fmap (Prelude.truncate :: Scientific -> Int) . asNumber)
    , ("int_ratio", fromFunction . variadicNumericFunc 1 $ fromIntegral . intRatio . Prelude.map Prelude.floor)
    , ("is_lt", fromFunction gfnLess)
    , ("iterable", fromFunction . unaryFunc $ toGVal . (\x -> isList x || isDict x))
    , ("json", fromFunction gfnJSON)
    , ("length", fromFunction gfnLength)
    , ("le", fromFunction gfnLessEquals)
    , ("less", fromFunction gfnLess)
    , ("lessthan", fromFunction gfnLess)
    , ("lessEquals", fromFunction gfnLessEquals)
    , ("lt", fromFunction gfnLess)
    , ("map", fromFunction gfnMap)
    , ("modulo", fromFunction . variadicNumericFunc 1 $ fromIntegral . modulo . Prelude.map Prelude.floor)
    , ("ne", fromFunction gfnNEquals)
    , ("nequals", fromFunction gfnNEquals)
    , ("num", fromFunction . unaryFunc $ toGVal . asNumber)
    , ("odd", fromFunction gfnOdd)
    , ("partial", fromFunction gfnPartial)
    , ("printf", fromFunction gfnPrintf)
    , ("product", fromFunction . variadicNumericFunc 1 $ Prelude.product)
    , ("ratio", fromFunction . variadicNumericFunc 1 $ Scientific.fromFloatDigits . ratio . Prelude.map Scientific.toRealFloat)
    , ("replace", fromFunction gfnReplace)
    , ("round", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.round)
    , ("show", fromFunction . unaryFunc $ fromString . show)
    , ("slice", fromFunction gfnSlice)
    , ("sort", fromFunction gfnSort)
    , ("split", fromFunction gfnSplit)
    , ("str", fromFunction . unaryFunc $ toGVal . asText)
    , ("sum", fromFunction . variadicNumericFunc 0 $ Prelude.sum)
    , ("truncate", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.truncate)
    , ("urlencode", fromFunction gfnUrlEncode)
    , ("upper", fromFunction . variadicStringFunc $ mconcat . Prelude.map Text.toUpper)
    , ("lower", fromFunction . variadicStringFunc $ mconcat . Prelude.map Text.toLower)
    , ("throw", fromFunction gfnThrow)
    , ("zip", fromFunction gfnZip)
    , ("zipwith", fromFunction gfnZipWith)

    -- Tests/predicates

    , ("in", fromFunction gfnIn)
    , ("escaped", fromFunction gfnEscaped)

    , ("regex", gfoRegex)

    -- TODO: sameas (predicate)
    -- NOTE that this test doesn't make sense in a host language where pointers
    -- are transparent - in Haskell, we simply don't care whether two values
    -- share a memory location or not, and whether they do or not might even
    -- depend on build flags.

    -- TODO: mapping (predicate)
    -- TODO: none (predicate)
    -- TODO: number (predicate)
    -- TODO: sequence (predicate)
    -- TODO: string (predicate)
    -- TODO: callable

    -- TODO: defined (predicate)
    -- TODO: undefined (predicate)
    -- NOTE that @defined@ cannot actually be written as a function. See
    -- issue #33.

    -- TODO: lower (predicate)
    -- TODO: upper (predicate)
    ]

-- | Simplified interface to render a ginger template \"into\" a monad.
--
-- @easyRenderM emit context template@ renders the @template@ with the
-- given @context@ object (which should represent some sort of
-- dictionary-like object) by feeding any output to the @emit@ function.
easyRenderM :: ( Monad m
               , ContextEncodable h
               , Monoid h
               , ToGVal (Run p m h) v
               , ToGVal (Run p m h) h
               , ToGVal (Run p m h) p
               )
            => (h -> m ())
            -> v
            -> Template p
            -> m (Either (RuntimeError p) (GVal (Run p m h)))
easyRenderM emit context template =
    runGingerT (easyContext emit context) template

-- | Simplified interface to render a ginger template in a pure fashion.
--
-- @easyRender context template@ renders the @template@ with the
-- given @context@ object (which should represent some sort of
-- dictionary-like object) by returning the concatenated output.
easyRender :: ( ContextEncodable h
              , Monoid h
              , ToGVal (Run p (Writer h) h) v
              , ToGVal (Run p (Writer h) h) h
              , ToGVal (Run p (Writer h) h) p
              )
           => v
           -> Template p
           -> h
easyRender context template =
    execWriter $ easyRenderM tell context template

-- | Purely expand a Ginger template. The underlying carrier monad is 'Writer'
-- 'h', which is used to collect the output and render it into a 'h'
-- value.
runGinger :: ( ToGVal (Run p (Writer h) h) h
             , ToGVal (Run p (Writer h) h) p
             , Monoid h
             )
          => GingerContext p (Writer h) h
          -> Template p
          -> h
runGinger context template =
    execWriter $ runGingerT context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad.
runGingerT :: ( ToGVal (Run p m h) h
              , ToGVal (Run p m h) p
              , Monoid h
              , Monad m
              )
           => GingerContext p m h
           -> Template p
           -> m (Either (RuntimeError p) (GVal (Run p m h)))
runGingerT context tpl =
    runReaderT (evalStateT (runExceptT (runTemplate tpl)) (defRunState tpl)) context

-- | Find the effective base template of an inheritance chain
baseTemplate :: Template p -> Template p
baseTemplate t =
    case templateParent t of
        Nothing -> t
        Just p -> baseTemplate p

-- | Run a template.
runTemplate :: ( ToGVal (Run p m h) h
               , ToGVal (Run p m h) p
               , Monoid h
               , Monad m
               )
            => Template p
            -> Run p m h (GVal (Run p m h))
runTemplate =
    runStatement . templateBody . baseTemplate

-- | Run an action within a different template context.
withTemplate :: Monad m
             => Template p
             -> Run p m h a
             -> Run p m h a
withTemplate tpl a = do
    oldTpl <- gets rsCurrentTemplate
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentTemplate = tpl, rsCurrentBlockName = Nothing })
    result <- withSourcePos (annotation tpl) a
    modify (\s -> s { rsCurrentTemplate = oldTpl, rsCurrentBlockName = oldBlockName })
    return result

-- | Run an action within a block context
withBlockName :: Monad m
              => VarName
              -> Run p m h a
              -> Run p m h a
withBlockName blockName a = do
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentBlockName = Just blockName })
    result <- a
    modify (\s -> s { rsCurrentBlockName = oldBlockName })
    return result

lookupBlock :: Monad m => VarName -> Run p m h (Block p)
lookupBlock blockName = do
    tpl <- gets rsCurrentTemplate
    let blockMay = resolveBlock blockName tpl
    case blockMay of
        Nothing -> throwHere $ UndefinedBlockError blockName
        Just block -> return block
    where
        resolveBlock :: VarName -> Template p -> Maybe (Block p)
        resolveBlock name tpl =
            case HashMap.lookup name (templateBlocks tpl) of
                Just block ->
                    return block -- Found it!
                Nothing ->
                    templateParent tpl >>= resolveBlock name

-- | Run one statement.
runStatement :: forall m h p
              . ( ToGVal (Run p m h) h
                , ToGVal (Run p m h) p
                , Monoid h
                , Monad m
                )
             => Statement p
             -> Run p m h (GVal (Run p m h))
runStatement stmt =
    withSourcePos
        (annotation stmt)
        (runStatement' stmt)

runStatement' :: forall m h p
              . ( ToGVal (Run p m h) h
                , ToGVal (Run p m h) p
                , Monoid h
                , Monad m
                )
             => Statement p
             -> Run p m h (GVal (Run p m h))
runStatement' (NullS _) =
    return def
runStatement' (MultiS _ xs) =
    forM xs runStatement >>= \case
        [] -> return def
        rvals -> return $ List.last rvals
runStatement' (LiteralS _ html) =
    echo (toGVal html) >> return def
runStatement' (InterpolationS _ expr) =
    runExpression expr >>= echo >> return def
runStatement' (ExpressionS _ expr) =
    runExpression expr
runStatement' (IfS _ condExpr true false) = do
    cond <- runExpression condExpr
    runStatement $ if toBoolean cond then true else false

runStatement' (IndentS _ expr body) = do
    i <- runExpression expr
    encode <- asks contextEncode
    let istr = encode i
    indented istr $ runStatement body

runStatement' (SwitchS _ pivotExpr cases defBranch) = do
    pivot <- runExpression pivotExpr
    let branches =
            [ \cont -> do
                cond <- runExpression condExpr
                if pivot `looseEquals` cond
                    then runStatement body
                    else cont
            | (condExpr, body)
            <- cases
            ] ++
            [ Prelude.const $ runStatement defBranch ]
    go branches
    where
        go :: [ Run p m h (GVal (Run p m h)) -> Run p m h (GVal (Run p m h)) ]
           -> Run p m h (GVal (Run p m h))
        go [] = return def
        go (x:xs) = x (go xs)

runStatement' (SetVarS _ name valExpr) = do
    val <- runExpression valExpr
    setVar name val
    return def

runStatement' (DefMacroS _ name macro) = do
    let val = macroToGVal macro
    setVar name val
    return def

runStatement' (BlockRefS _ blockName) = do
    block <- lookupBlock blockName
    withBlockName blockName $
        runStatement (blockBody block)

runStatement' (ScopedS _ body) = withLocalScope runInner
    where
        runInner :: Monad m => Run p m h (GVal (Run p m h))
        runInner = runStatement body

runStatement' (ForS _ varNameIndex varNameValue itereeExpr body) = do
    let go :: Int -> GVal (Run p m h) -> Run p m h (GVal (Run p m h))
        go recursionDepth iteree = do
            iterPairs <- if isJust (asDictItems iteree)
                then return [ (toGVal k, v) | (k, v) <- fromMaybe [] (asDictItems iteree) ]
                else case asList iteree of
                  Just items -> return $ Prelude.zip (Prelude.map toGVal ([0..] :: [Int])) items
                  Nothing -> do
                    warn $ TypeError ["list", "dictionary"] (Just $ tshow iteree)
                    return []
            let numItems :: Int
                numItems = Prelude.length iterPairs
                cycle :: Int -> [(Maybe Text, GVal (Run p m h))] -> Run p m h (GVal (Run p m h))
                cycle index args = return
                                 . fromMaybe def
                                 . headMay
                                 . Prelude.drop (index `Prelude.mod` Prelude.length args)
                                 . fmap snd
                                 $ args
                loop :: [(Maybe Text, GVal (Run p m h))] -> Run p m h (GVal (Run p m h))
                loop [] = throwHere $ ArgumentsError (Just "loop") "at least one argument is required"
                loop ((_, loopee):_) = go (Prelude.succ recursionDepth) loopee
                iteration :: (Int, (GVal (Run p m h), GVal (Run p m h)))
                          -> Run p m h (GVal (Run p m h))
                iteration (index, (key, value)) = do
                    setVar varNameValue value
                    setVar "loop" $
                        (dict [ "index" ~> Prelude.succ index
                             , "index0" ~> index
                             , "revindex" ~> (numItems - index)
                             , "revindex0" ~> (numItems - index - 1)
                             , "depth" ~> Prelude.succ recursionDepth
                             , "depth0" ~> recursionDepth
                             , "first" ~> (index == 0)
                             , "last" ~> (Prelude.succ index == numItems)
                             , "length" ~> numItems
                             , "cycle" ~> fromFunction (cycle index)
                             ])
                             { asFunction = Just loop }
                    case varNameIndex of
                        Nothing -> return def
                        Just n -> setVar n key
                    runStatement body
            (withLocalScope $ forM (Prelude.zip [0..] iterPairs) iteration) >>= \case
                [] -> return def
                rvals -> return $ List.last rvals
    runExpression itereeExpr >>= go 0

runStatement' (PreprocessedIncludeS _ tpl) =
    withTemplate tpl $ runTemplate tpl

runStatement' (TryCatchS _ tryS catchesS finallyS) = do
    result <- (runStatement tryS) `catchError` handle catchesS
    runStatement finallyS
    return result
    where
        handle [] e = return def
        handle ((Catch whatMay varNameMay catchS):catches) e = do
            let what = runtimeErrorWhat e
            if whatMay == Just what || whatMay == Nothing
                then
                    withLocalScope $ do
                        case varNameMay of
                            Nothing -> return ()
                            Just varName -> setVar varName (toGVal e)
                        runStatement catchS
                else
                    handle catches e

-- | Deeply magical function that converts a 'Macro' into a Function.
macroToGVal :: forall m h p
             . ( ToGVal (Run p m h) h
               , ToGVal (Run p m h) p
               , Monoid h
               , Monad m
               ) => Macro p -> GVal (Run p m h)
macroToGVal (Macro argNames body) =
    fromFunction f
    where
        f :: Function (Run p m h)
        -- Establish a local state to not contaminate the parent scope
        -- with function arguments and local variables, and;
        -- Establish a local context, where we override the HTML writer,
        -- rewiring it to append any output to the state's capture.
        f args =
            withLocalState . local (\c -> c { contextWrite = appendCapture }) $ do
                clearCapture
                forM_ (HashMap.toList matchedArgs) (uncurry setVar)
                setVar "varargs" . toGVal $ positionalArgs
                setVar "kwargs" . toGVal $ namedArgs
                runStatement body
                -- At this point, we're still inside the local state, so the
                -- capture contains the macro's output; we now simply return
                -- the capture as the function's return value.
                toGVal <$> fetchCapture
                where
                    matchArgs' :: [(Maybe Text, GVal (Run p m h))] -> (HashMap Text (GVal (Run p m h)), [GVal (Run p m h)], HashMap Text (GVal (Run p m h)))
                    matchArgs' = matchFuncArgs argNames
                    (matchedArgs, positionalArgs, namedArgs) = matchArgs' args

-- | Run p (evaluate) an expression and return its value into the Run p monad
runExpression expr =
    withSourcePos
        (annotation expr)
        (runExpression' expr)

runExpression' (StringLiteralE _ str) = return . toGVal $ str
runExpression' (NumberLiteralE _ n) = return . toGVal $ n
runExpression' (BoolLiteralE _ b) = return . toGVal $ b
runExpression' (NullLiteralE _) = return def
runExpression' (VarE _ key) = getVar key
runExpression' (ListE _ xs) = toGVal <$> forM xs runExpression
runExpression' (ObjectE _ xs) = do
    items <- forM xs $ \(a, b) -> do
        l <- asText <$> runExpression a
        r <- runExpression b
        return (l, r)
    return . toGVal . HashMap.fromList $ items
runExpression' (MemberLookupE _ baseExpr indexExpr) = do
    base <- runExpression baseExpr
    index <- runExpression indexExpr
    warnFromMaybe (IndexError $ tshow (asText index)) def $
        lookupLoose index base
runExpression' (CallE _ funcE argsEs) = do
    args <- forM argsEs $
        \(argName, argE) -> (argName,) <$> runExpression argE
    e <- runExpression funcE
    let func = toFunction e
    case func of
        Nothing -> do
            warn NotAFunctionError
            return def
        Just f -> f args
runExpression' (LambdaE _ argNames body) = do
    let fn args = withLocalScope $ do
            forM_ (Prelude.zip argNames (fmap snd args)) $ uncurry setVar
            runExpression body
    return $ fromFunction fn
runExpression' (TernaryE _ condition yes no) = do
    condVal <- runExpression condition
    let expr = if asBoolean condVal then yes else no
    runExpression expr
runExpression' (DoE _ stmt) =
    runStatement stmt

-- | Helper function to output a HTML value using whatever print function the
-- context provides.
echo :: (Monad m, Monoid h)
     => GVal (Run p m h) -> Run p m h ()
echo src = do
    e <- asks contextEncode
    p <- asks contextWrite
    asks contextNewlines >>= \case
        Nothing ->
            p . e $ src
        Just newlines -> do
            indentation <- fromMaybe [] <$> gets rsIndentation
            let ls = splitLines newlines $ e src
                indent = mconcat . List.reverse $ indentation
            forM_ ls $ \l -> do
                atLineStart <- gets rsAtLineStart
                if atLineStart
                    then p $ indent <> l
                    else p l
                modify $ \state -> state {
                    rsAtLineStart = endsWithNewline newlines l
                }

indented :: (Monad m, Monoid h)
         => h
         -> Run p m h a
         -> Run p m h a
indented i action = do
    pushIndent i *> action <* popIndent

pushIndent :: (Monad m, Monoid h)
           => h
           -> Run p m h ()
pushIndent i =
    modify $ \state ->
        state { rsIndentation = increaseIndent i (rsIndentation state) }
popIndent :: (Monad m, Monoid h)
           => Run p m h ()
popIndent =
    modify $ \state ->
        state { rsIndentation = decreaseIndent (rsIndentation state) }

increaseIndent :: a -> Maybe [a] -> Maybe [a]
increaseIndent _ Nothing = Just []
increaseIndent x (Just xs) = Just (x:xs)

decreaseIndent :: Maybe [a] -> Maybe [a]
decreaseIndent Nothing = Nothing
decreaseIndent (Just []) = Nothing
decreaseIndent (Just (x:xs)) = Just xs

defRunState :: forall m h p
             . ( ToGVal (Run p m h) h
               , ToGVal (Run p m h) p
               , Monoid h
               , Monad m
               )
            => Template p
            -> RunState p m h
defRunState tpl =
    RunState
        { rsScope = HashMap.fromList defaultScope
        , rsCapture = mempty
        , rsCurrentTemplate = tpl
        , rsCurrentBlockName = Nothing
        , rsIndentation = Nothing
        , rsAtLineStart = True
        , rsCurrentSourcePos = annotation tpl
        }

gfnThrow :: ( Monad m
            , Monoid h
            , ToGVal (Run p m h) h
            , ToGVal (Run p m h) p
            )
         => Function (Run p m h)
gfnThrow args =
    throwHere (RuntimeError . mconcat . fmap (asText . snd) $ args)

gfnEval :: ( Monad m
           , Monoid h
           , ToGVal (Run p m h) h
           , ToGVal (Run p m h) p
           )
        => Function (Run p m h)
gfnEval args =
    let extracted =
            extractArgsDefL
                [ ("src", def)
                , ("context", def)
                ]
                args
    in case extracted of
        Left _ -> throwHere $ ArgumentsError (Just "eval") "expected: (src, context)"
        Right [gSrc, gContext] -> do
            result' <- parseGinger
                (Prelude.const . return $ Nothing) -- include resolver
                Nothing -- source name
                (asText gSrc) -- source code
            pos <- gets rsCurrentSourcePos
            let result = fmap (Prelude.const pos) <$> result'
            tpl <- case result of
                Left err -> throwHere $ EvalParseError err
                Right t -> return t
            let localLookup varName = return $
                    lookupLooseDef def (toGVal varName) gContext
                localContext c = c
                    { contextWrite = appendCapture
                    , contextLookup = localLookup
                    }
            withLocalState $ do
                put $ defRunState tpl
                local localContext $ do
                    clearCapture
                    runStatement $ templateBody tpl
                    -- At this point, we're still inside the local state, so the
                    -- capture contains the macro's output; we now simply return
                    -- the capture as the function's return value.
                    toGVal <$> fetchCapture
