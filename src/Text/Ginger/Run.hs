{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
-- | Execute Ginger templates in an arbitrary monad.
--
-- Usage example:
--
-- > render :: Template -> Text -> Text -> Text
-- > render template -> username imageURL = do
-- >    let contextLookup varName =
-- >            case varName of
-- >                "username" -> toGVal username
-- >                "imageURL" -> toGVal imageURL
-- >                _ -> def -- def for GVal is equivalent to a NULL value
-- >        context = makeContext contextLookup
-- >    in htmlSource $ runGinger context template
module Text.Ginger.Run
( runGingerT
, runGinger
, GingerContext
, makeContext
, makeContextM
, makeContext'
, makeContextM'
, makeContextHtml
, makeContextHtmlM
, makeContextText
, makeContextTextM
, Run, liftRun, liftRun2
)
where

import Prelude ( (.), ($), (==), (/=)
               , (>), (<), (>=), (<=)
               , (+), (-), (*), (/), div
               , (||), (&&)
               , (++)
               , Show, show
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , Int
               , fromIntegral, floor
               , not
               , show
               , uncurry
               , seq
               , snd
               )
import qualified Prelude
import Data.Maybe (fromMaybe, isJust)
import qualified Data.List as List
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.GVal

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
import Data.Scientific (Scientific)
import Data.Scientific as Scientific
import Data.Default (def)
import Safe (readMay)
import Network.HTTP.Types (urlEncode)

-- | Execution context. Determines how to look up variables from the
-- environment, and how to write out template output.
data GingerContext m h
    = GingerContext
        { contextLookup :: VarName -> Run m h (GVal (Run m h))
        , contextWrite :: h -> Run m h ()
        , contextEncode :: GVal (Run m h) -> h
        }

contextWriteEncoded :: GingerContext m h -> GVal (Run m h) -> Run m h ()
contextWriteEncoded context =
    contextWrite context . contextEncode context

data RunState m h
    = RunState
        { rsScope :: HashMap VarName (GVal (Run m h))
        , rsCapture :: h
        , rsCurrentTemplate :: Template -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
        }

unaryFunc :: forall m h. (Monad m) => (GVal (Run m h) -> GVal (Run m h)) -> Function (Run m h)
unaryFunc f [] = return def
unaryFunc f ((_, x):[]) = return (f x)

ignoreArgNames :: ([a] -> b) -> ([(c, a)] -> b)
ignoreArgNames f args = f (Prelude.map snd args)

variadicNumericFunc :: Monad m => Scientific -> ([Scientific] -> Scientific) -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
variadicNumericFunc zero f args =
    return . toGVal . f $ args'
    where
        args' :: [Scientific]
        args' = Prelude.map (fromMaybe zero . asNumber . snd) args

unaryNumericFunc :: Monad m => Scientific -> (Scientific -> Scientific) -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
unaryNumericFunc zero f args =
    return . toGVal . f $ args'
    where
        args' :: Scientific
        args' = case args of
                    [] -> 0
                    (arg:_) -> fromMaybe zero . asNumber . snd $ arg

variadicStringFunc :: Monad m => ([Text] -> Text) -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
variadicStringFunc f args =
    return . toGVal . f $ args'
    where
        args' :: [Text]
        args' = Prelude.map (asText . snd) args

defRunState :: forall m h. (Monoid h, Monad m) => Template -> RunState m h
defRunState tpl =
    RunState
        { rsScope = HashMap.fromList scope
        , rsCapture = mempty
        , rsCurrentTemplate = tpl
        , rsCurrentBlockName = Nothing
        }
    where
        scope :: [(Text, GVal (Run m h))]
        scope =
            [ ("raw", fromFunction gfnRawHtml)
            , ("abs", fromFunction . unaryNumericFunc 0 $ Prelude.abs)
            , ("any", fromFunction gfnAny)
            , ("all", fromFunction gfnAll)
            -- TODO: batch
            , ("ceil", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.ceiling)
            , ("capitalize", fromFunction . variadicStringFunc $ mconcat . Prelude.map capitalize)
            , ("center", fromFunction gfnCenter)
            , ("concat", fromFunction . variadicStringFunc $ mconcat)
            , ("contains", fromFunction gfnContains)
            , ("default", fromFunction gfnDefault)
            , ("difference", fromFunction . variadicNumericFunc 0 $ difference)
            , ("equals", fromFunction gfnEquals)
            , ("nequals", fromFunction gfnNEquals)
            , ("greaterEquals", fromFunction gfnGreaterEquals)
            , ("lessEquals", fromFunction gfnLessEquals)
            , ("greater", fromFunction gfnGreater)
            , ("less", fromFunction gfnLess)
            , ("floor", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.floor)
            , ("int", fromFunction . unaryFunc $ toGVal . (fmap (Prelude.truncate :: Scientific -> Int)) . asNumber)
            , ("int_ratio", fromFunction . variadicNumericFunc 1 $ fromIntegral . intRatio . Prelude.map Prelude.floor)
            , ("iterable", fromFunction . unaryFunc $ toGVal . (\x -> isList x || isDict x))
            , ("length", fromFunction . unaryFunc $ toGVal . length)
            , ("modulo", fromFunction . variadicNumericFunc 1 $ fromIntegral . modulo . Prelude.map Prelude.floor)
            , ("num", fromFunction . unaryFunc $ toGVal . asNumber)
            , ("product", fromFunction . variadicNumericFunc 1 $ Prelude.product)
            , ("ratio", fromFunction . variadicNumericFunc 1 $ Scientific.fromFloatDigits . ratio . Prelude.map Scientific.toRealFloat)
            , ("round", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.round)
            , ("show", fromFunction . unaryFunc $ fromString . show)
            , ("str", fromFunction . unaryFunc $ toGVal . asText)
            , ("sum", fromFunction . variadicNumericFunc 0 $ Prelude.sum)
            , ("truncate", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.truncate)
            , ("urlencode", fromFunction $ gfnUrlEncode)
            , ("sort", fromFunction $ gfnSort)
            ]

        gfnRawHtml :: Function (Run m h)
        gfnRawHtml = unaryFunc (toGVal . unsafeRawHtml . asText)

        gfnUrlEncode :: Function (Run m h)
        gfnUrlEncode =
            unaryFunc
                ( toGVal
                . Text.pack
                . UTF8.toString
                . urlEncode True
                . UTF8.fromString
                . Text.unpack
                . asText
                )

        gfnDefault :: Function (Run m h)
        gfnDefault [] = return def
        gfnDefault ((_, x):xs)
            | asBoolean x = return x
            | otherwise = gfnDefault xs

        gfnAny :: Function (Run m h)
        gfnAny xs = return . toGVal $ Prelude.any (asBoolean . snd) xs

        gfnAll :: Function (Run m h)
        gfnAll xs = return . toGVal $ Prelude.all (asBoolean . snd) xs

        gfnEquals :: Function (Run m h)
        gfnEquals [] = return $ toGVal True
        gfnEquals (x:[]) = return $ toGVal True
        gfnEquals (x:xs) =
            return . toGVal $ Prelude.all ((snd x `looseEquals`) . snd) xs

        gfnNEquals :: Function (Run m h)
        gfnNEquals [] = return $ toGVal True
        gfnNEquals (x:[]) = return $ toGVal True
        gfnNEquals (x:xs) =
            return . toGVal $ Prelude.any (not . (snd x `looseEquals`) . snd) xs

        gfnContains :: Function (Run m h)
        gfnContains [] = return $ toGVal False
        gfnContains (list:elems) =
            let rawList = fromMaybe [] . asList . snd $ list
                rawElems = fmap snd elems
                e `isInList` xs = Prelude.any (looseEquals e) xs
                es `areInList` xs = Prelude.all (`isInList` xs) es
            in return . toGVal $ rawElems `areInList` rawList

        looseEquals :: GVal (Run m h) -> GVal (Run m h) -> Bool
        looseEquals a b
            | isJust (asFunction a) || isJust (asFunction b) = False
            | isJust (asList a) /= isJust (asList b) = False
            | isJust (asDictItems a) /= isJust (asDictItems b) = False
            -- Both numbers: do numeric comparison
            | isJust (asNumber a) && isJust (asNumber b) = asNumber a == asNumber b
            -- If either is NULL, the other must be falsy
            | isNull a || isNull b = asBoolean a == asBoolean b
            | otherwise = asText a == asText b

        gfnLess :: Function (Run m h)
        gfnLess [] = return . toGVal $ False
        gfnLess xs' =
            let xs = fmap snd xs'
            in return . toGVal $
                Prelude.all (== Just True) (Prelude.zipWith less xs (Prelude.tail xs))

        gfnGreater :: Function (Run m h)
        gfnGreater [] = return . toGVal $ False
        gfnGreater xs' =
            let xs = fmap snd xs'
            in return . toGVal $
                Prelude.all (== Just True) (Prelude.zipWith greater xs (Prelude.tail xs))

        gfnLessEquals :: Function (Run m h)
        gfnLessEquals [] = return . toGVal $ False
        gfnLessEquals xs' =
            let xs = fmap snd xs'
            in return . toGVal $
                Prelude.all (== Just True) (Prelude.zipWith lessEq xs (Prelude.tail xs))

        gfnGreaterEquals :: Function (Run m h)
        gfnGreaterEquals [] = return . toGVal $ False
        gfnGreaterEquals xs' =
            let xs = fmap snd xs'
            in return . toGVal $
                Prelude.all (== Just True) (Prelude.zipWith greaterEq xs (Prelude.tail xs))

        less :: GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
        less a b = (<) <$> asNumber a <*> asNumber b

        greater :: GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
        greater a b = (>) <$> asNumber a <*> asNumber b

        lessEq :: GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
        lessEq a b = (<=) <$> asNumber a <*> asNumber b

        greaterEq :: GVal (Run m h) -> GVal (Run m h) -> Maybe Bool
        greaterEq a b = (>=) <$> asNumber a <*> asNumber b

        difference :: Prelude.Num a => [a] -> a
        difference (x:xs) = x - Prelude.sum xs
        difference [] = 0

        ratio :: (Show a, Prelude.Fractional a, Prelude.Num a) => [a] -> a
        ratio (x:xs) = x / Prelude.product xs
        ratio [] = 0

        intRatio :: (Prelude.Integral a, Prelude.Num a) => [a] -> a
        intRatio (x:xs) = x `Prelude.div` Prelude.product xs
        intRatio [] = 0

        modulo :: (Prelude.Integral a, Prelude.Num a) => [a] -> a
        modulo (x:xs) = x `Prelude.mod` Prelude.product xs
        modulo [] = 0

        capitalize :: Text -> Text
        capitalize txt = Text.toUpper (Text.take 1 txt) <> Text.drop 1 txt

        gfnCenter :: Function (Run m h)
        gfnCenter [] = gfnCenter [(Nothing, toGVal ("" :: Text))]
        gfnCenter (x:[]) = gfnCenter [x, (Nothing, toGVal (80 :: Int))]
        gfnCenter (x:y:[]) = gfnCenter [x, y, (Nothing, toGVal (" " :: Text))]
        gfnCenter ((_, s):(_, w):(_, pad):_) =
            return . toGVal $ center (asText s) (fromMaybe 80 $ Prelude.truncate <$> asNumber w) (asText pad)

        gfnSort :: Function (Run m h)
        gfnSort [] = return def
        gfnSort ((_,sortee):args) = do
            let sortKeyMay = asText <$> List.lookup (Just "by") args
                sortReverse = fromMaybe False $ asBoolean <$> List.lookup (Just "reverse") args
                baseComparer :: (GVal (Run m h)) -> (GVal (Run m h)) -> Prelude.Ordering
                baseComparer = \a b -> Prelude.compare (asText a) (asText b)
                extractKey :: Text -> GVal (Run m h) -> GVal (Run m h)
                extractKey k g = fromMaybe def $ do
                    l <- asLookup g
                    l k
            if isDict sortee
                then do
                    let comparer' :: (Text, GVal (Run m h)) -> (Text, GVal (Run m h)) -> Prelude.Ordering
                        comparer' = case sortKeyMay of
                            Nothing -> \(_, a) (_, b) -> baseComparer a b
                            Just "__key" -> \(a, _) (b, _) -> Prelude.compare a b
                            Just k -> \(_, a) (_, b) ->
                                baseComparer
                                    (extractKey k a) (extractKey k b)
                        comparer =
                            if sortReverse
                                then \a b -> comparer' b a
                                else comparer'
                    return . toGVal $ List.sortBy comparer (fromMaybe [] $ asDictItems sortee)
                else do
                    let comparer' :: (GVal (Run m h)) -> (GVal (Run m h)) -> Prelude.Ordering
                        comparer' = case sortKeyMay of
                            Nothing ->
                                baseComparer
                            Just k -> \a b ->
                                baseComparer
                                    (extractKey k a) (extractKey k b)
                    let comparer =
                            if sortReverse
                                then \a b -> comparer' b a
                                else comparer'
                    return . toGVal $ List.sortBy comparer (fromMaybe [] $ asList sortee)

        center :: Text -> Prelude.Int -> Text -> Text
        center str width pad =
            if Text.length str Prelude.>= width
                then str
                else paddingL <> str <> paddingR
            where
                chars = width - Text.length str
                charsL = chars `div` 2
                charsR = chars - charsL
                repsL = Prelude.succ charsL `div` Text.length pad
                paddingL = Text.take charsL . Text.replicate repsL $ pad
                repsR = Prelude.succ charsR `div` Text.length pad
                paddingR = Text.take charsR . Text.replicate repsR $ pad

-- | Create an execution context for runGingerT.
-- Takes a lookup function, which returns ginger values into the carrier monad
-- based on a lookup key, and a writer function (outputting HTML by whatever
-- means the carrier monad provides, e.g. @putStr@ for @IO@, or @tell@ for
-- @Writer@s).
makeContextM' :: (Monad m, Functor m)
             => (VarName -> Run m h (GVal (Run m h)))
             -> (h -> m ())
             -> (GVal (Run m h) -> h)
             -> GingerContext m h
makeContextM' lookupFn writeFn encodeFn =
    GingerContext
        { contextLookup = lookupFn
        , contextWrite = liftRun2 writeFn
        , contextEncode = encodeFn
        }

liftLookup :: (Monad m, ToGVal (Run m h) v) => (VarName -> m v) -> VarName -> Run m h (GVal (Run m h))
liftLookup f k = do
    v <- liftRun $ f k
    return . toGVal $ v

-- | Create an execution context for runGinger.
-- The argument is a lookup function that maps top-level context keys to ginger
-- values. 'makeContext' is a specialized version of 'makeContextM', targeting
-- the 'Writer' 'Html' monad (which is what is used for the non-monadic
-- template interpreter 'runGinger').
--
-- The type of the lookup function may look intimidating, but in most cases,
-- marshalling values from Haskell to Ginger is a matter of calling 'toGVal'
-- on them, so the 'GVal (Run (Writer Html))' part can usually be ignored.
-- See the 'Text.Ginger.GVal' module for details.
makeContext' :: Monoid h
            => (VarName -> GVal (Run (Writer h) h))
            -> (GVal (Run (Writer h) h) -> h)
            -> GingerContext (Writer h) h
makeContext' lookupFn encodeFn =
    makeContextM'
        (return . lookupFn)
        tell
        encodeFn

{-#DEPRECATED makeContext "Compatibility alias for makeContextHtml" #-}
makeContext :: (VarName -> GVal (Run (Writer Html) Html))
            -> GingerContext (Writer Html) Html
makeContext = makeContextHtml

{-#DEPRECATED makeContextM "Compatibility alias for makeContextHtmlM" #-}
makeContextM :: (Monad m, Functor m)
             => (VarName -> Run m Html (GVal (Run m Html)))
             -> (Html -> m ())
             -> GingerContext m Html
makeContextM = makeContextHtmlM

makeContextHtml :: (VarName -> GVal (Run (Writer Html) Html))
                -> GingerContext (Writer Html) Html
makeContextHtml l = makeContext' l toHtml

makeContextHtmlM :: (Monad m, Functor m)
                 => (VarName -> Run m Html (GVal (Run m Html)))
                 -> (Html -> m ())
                 -> GingerContext m Html
makeContextHtmlM l w = makeContextM' l w toHtml

makeContextText :: (VarName -> GVal (Run (Writer Text) Text))
                -> GingerContext (Writer Text) Text
makeContextText l = makeContext' l asText

makeContextTextM :: (Monad m, Functor m)
                 => (VarName -> Run m Text (GVal (Run m Text)))
                 -> (Text -> m ())
                 -> GingerContext m Text
makeContextTextM l w = makeContextM' l w asText

-- | Purely expand a Ginger template. The underlying carrier monad is 'Writer'
-- 'h', which is used to collect the output and render it into a 'h'
-- value.
runGinger :: (ToGVal (Run (Writer h) h) h, Monoid h) => GingerContext (Writer h) h -> Template -> h
runGinger context template = execWriter $ runGingerT context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad.
runGingerT :: (ToGVal (Run m h) h, Monoid h, Monad m, Functor m) => GingerContext m h -> Template -> m ()
runGingerT context tpl = runReaderT (evalStateT (runTemplate tpl) (defRunState tpl)) context

-- | Internal type alias for our template-runner monad stack.
type Run m h = StateT (RunState m h) (ReaderT (GingerContext m h) m)

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run m h a
liftRun = lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run m h b
liftRun2 f x = liftRun $ f x

-- | Find the effective base template of an inheritance chain
baseTemplate :: Template -> Template
baseTemplate t =
    case templateParent t of
        Nothing -> t
        Just p -> baseTemplate p

-- | Run a template.
runTemplate :: (ToGVal (Run m h) h, Monoid h, Monad m, Functor m) => Template -> Run m h ()
runTemplate = runStatement . templateBody . baseTemplate

-- | Run an action within a different template context.
withTemplate :: (Monad m, Functor m) => Template -> Run m h a -> Run m h a
withTemplate tpl a = do
    oldTpl <- gets rsCurrentTemplate
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentTemplate = tpl, rsCurrentBlockName = Nothing })
    result <- a
    modify (\s -> s { rsCurrentTemplate = oldTpl, rsCurrentBlockName = oldBlockName })
    return result

-- | Run an action within a block context
withBlockName :: (Monad m, Functor m) => VarName -> Run m h a -> Run m h a
withBlockName blockName a = do
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentBlockName = Just blockName })
    result <- a
    modify (\s -> s { rsCurrentBlockName = oldBlockName })
    return result

lookupBlock :: (Monad m, Functor m) => VarName -> Run m h Block
lookupBlock blockName = do
    tpl <- gets rsCurrentTemplate
    let blockMay = resolveBlock blockName tpl
    case blockMay of
        Nothing -> fail $ "Block " <> (Text.unpack blockName) <> " not defined"
        Just block -> return block
    where
        resolveBlock :: VarName -> Template -> Maybe Block
        resolveBlock name tpl =
            case HashMap.lookup name (templateBlocks tpl) of
                Just block ->
                    return block -- Found it!
                Nothing ->
                    templateParent tpl >>= resolveBlock name

-- | Run one statement.
runStatement :: forall m h. (ToGVal (Run m h) h, Monoid h, Monad m, Functor m) => Statement -> Run m h ()
runStatement NullS = return ()
runStatement (MultiS xs) = forM_ xs runStatement
runStatement (LiteralS html) = echo (toGVal html)
runStatement (InterpolationS expr) = runExpression expr >>= echo
runStatement (IfS condExpr true false) = do
    cond <- runExpression condExpr
    runStatement $ if toBoolean cond then true else false

runStatement (SetVarS name valExpr) = do
    val <- runExpression valExpr
    setVar name val

runStatement (DefMacroS name macro) = do
    let val = macroToGVal macro
    setVar name val

runStatement (BlockRefS blockName) = do
    block <- lookupBlock blockName
    withBlockName blockName $
        runStatement (blockBody block)

runStatement (ScopedS body) = withLocalScope runInner
    where
        runInner :: (Functor m, Monad m) => Run m h ()
        runInner = runStatement body

runStatement (ForS varNameIndex varNameValue itereeExpr body) = do
    iteree <- runExpression itereeExpr
    let iterPairs =
            if isJust (asDictItems iteree)
                then [ (toGVal k, v) | (k, v) <- fromMaybe [] (asDictItems iteree) ]
                else Prelude.zip (Prelude.map toGVal ([0..] :: [Int])) (fromMaybe [] (asList iteree))
    withLocalScope $ forM_ iterPairs iteration
    where
        iteration (index, value) = do
            setVar varNameValue value
            case varNameIndex of
                Nothing -> return ()
                Just n -> setVar n index
            runStatement body

runStatement (PreprocessedIncludeS tpl) =
    withTemplate tpl $ runTemplate tpl

-- | Deeply magical function that converts a 'Macro' into a Function.
macroToGVal :: forall m h. (ToGVal (Run m h) h, Monoid h, Functor m, Monad m) => Macro -> GVal (Run m h)
macroToGVal (Macro argNames body) =
    fromFunction f
    where
        f :: Function (Run m h)
        -- Establish a local state to not contaminate the parent scope
        -- with function arguments and local variables, and;
        -- Establish a local context, where we override the HTML writer,
        -- rewiring it to append any output to the state's capture.
        f args =
            withLocalState . local (\c -> c { contextWrite = appendCapture }) $ do
                clearCapture
                forM (HashMap.toList matchedArgs) (uncurry setVar)
                setVar "varargs" . toGVal $ positionalArgs
                setVar "kwargs" . toGVal $ namedArgs
                runStatement body
                -- At this point, we're still inside the local state, so the
                -- capture contains the macro's output; we now simply return
                -- the capture as the function's return value.
                toGVal <$> fetchCapture
                where
                    matchArgs' :: [(Maybe Text, GVal (Run m h))] -> (HashMap Text (GVal (Run m h)), [GVal (Run m h)], HashMap Text (GVal (Run m h)))
                    matchArgs' = matchFuncArgs argNames
                    (matchedArgs, positionalArgs, namedArgs) = matchArgs' args


-- | Helper function to run a State action with a temporary state, reverting
-- to the old state after the action has finished.
withLocalState :: (Monad m, MonadState s m) => m a -> m a
withLocalState a = do
    s <- get
    r <- a
    put s
    return r

-- | Helper function to run a Scope action with a temporary scope, reverting
-- to the old scope after the action has finished.
withLocalScope :: (Monad m) => Run m h a -> Run m h a
withLocalScope a = do
    scope <- gets rsScope
    r <- a
    modify (\s -> s { rsScope = scope })
    return r

setVar :: Monad m => VarName -> GVal (Run m h) -> Run m h ()
setVar name val = do
    vars <- gets rsScope
    let vars' = HashMap.insert name val vars
    modify (\s -> s { rsScope = vars' })

getVar :: Monad m => VarName -> Run m h (GVal (Run m h))
getVar key = do
    vars <- gets rsScope
    case HashMap.lookup key vars of
        Just val ->
            return val
        Nothing -> do
            l <- asks contextLookup
            l key

clearCapture :: (Monoid h, Monad m) => Run m h ()
clearCapture = modify (\s -> s { rsCapture = mempty })

appendCapture :: (Monoid h, Monad m) => h -> Run m h ()
appendCapture h = modify (\s -> s { rsCapture = rsCapture s <> h })

fetchCapture :: Monad m => Run m h h
fetchCapture = gets rsCapture

-- | Run (evaluate) an expression and return its value into the Run monad
runExpression (StringLiteralE str) = return . toGVal $ str
runExpression (NumberLiteralE n) = return . toGVal $ n
runExpression (BoolLiteralE b) = return . toGVal $ b
runExpression (NullLiteralE) = return def
runExpression (VarE key) = getVar key
runExpression (ListE xs) = toGVal <$> forM xs runExpression
runExpression (ObjectE xs) = do
    items <- forM xs $ \(a, b) -> do
        l <- asText <$> runExpression a
        r <- runExpression b
        return (l, r)
    return . toGVal . HashMap.fromList $ items
runExpression (MemberLookupE baseExpr indexExpr) = do
    base <- runExpression baseExpr
    index <- runExpression indexExpr
    return . fromMaybe def . lookupLoose index $ base
runExpression (CallE funcE argsEs) = do
    args <- forM argsEs $
        \(argName, argE) -> (argName,) <$> runExpression argE
    func <- toFunction <$> runExpression funcE
    case func of
        Nothing -> return def
        Just f -> f args

-- | Helper function to output a HTML value using whatever print function the
-- context provides.
echo :: (Monad m, Functor m) => GVal (Run m h) -> Run m h ()
echo src = do
    e <- asks contextEncode
    p <- asks contextWrite
    p . e $ src
