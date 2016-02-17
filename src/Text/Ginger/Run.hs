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
, Run, liftRun, liftRun2
)
where

import Prelude ( (.), ($), (==), (/=)
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
data GingerContext m
    = GingerContext
        { contextLookup :: VarName -> Run m (GVal (Run m))
        , contextWriteHtml :: Html -> Run m ()
        }

data RunState m
    = RunState
        { rsScope :: HashMap VarName (GVal (Run m))
        , rsCapture :: Html
        , rsCurrentTemplate :: Template -- the template we are currently running
        , rsCurrentBlockName :: Maybe Text -- the name of the innermost block we're currently in
        }

unaryFunc :: forall m. (Monad m) => (GVal (Run m) -> GVal (Run m)) -> Function (Run m)
unaryFunc f [] = return def
unaryFunc f ((_, x):[]) = return (f x)

ignoreArgNames :: ([a] -> b) -> ([(c, a)] -> b)
ignoreArgNames f args = f (Prelude.map snd args)

variadicNumericFunc :: Monad m => Scientific -> ([Scientific] -> Scientific) -> [(Maybe Text, GVal (Run m))] -> Run m (GVal (Run m))
variadicNumericFunc zero f args =
    return . toGVal . f $ args'
    where
        args' :: [Scientific]
        args' = Prelude.map (fromMaybe zero . asNumber . snd) args

unaryNumericFunc :: Monad m => Scientific -> (Scientific -> Scientific) -> [(Maybe Text, GVal (Run m))] -> Run m (GVal (Run m))
unaryNumericFunc zero f args =
    return . toGVal . f $ args'
    where
        args' :: Scientific
        args' = case args of
                    [] -> 0
                    (arg:_) -> fromMaybe zero . asNumber . snd $ arg

variadicStringFunc :: Monad m => ([Text] -> Text) -> [(Maybe Text, GVal (Run m))] -> Run m (GVal (Run m))
variadicStringFunc f args =
    return . toGVal . f $ args'
    where
        args' :: [Text]
        args' = Prelude.map (asText . snd) args

defRunState :: forall m. Monad m => Template -> RunState m
defRunState tpl =
    RunState
        { rsScope = HashMap.fromList scope
        , rsCapture = html ""
        , rsCurrentTemplate = tpl
        , rsCurrentBlockName = Nothing
        }
    where
        scope :: [(Text, GVal (Run m))]
        scope =
            [ ("raw", fromFunction gfnRawHtml)
            , ("abs", fromFunction . unaryNumericFunc 0 $ Prelude.abs)
            -- TODO: batch
            , ("ceil", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.ceiling)
            , ("capitalize", fromFunction . variadicStringFunc $ mconcat . Prelude.map capitalize)
            , ("center", fromFunction gfnCenter)
            , ("concat", fromFunction . variadicStringFunc $ mconcat)
            , ("default", fromFunction gfnDefault)
            , ("difference", fromFunction . variadicNumericFunc 0 $ difference)
            , ("equals", fromFunction gfnEquals)
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
            ]

        gfnRawHtml :: Function (Run m)
        gfnRawHtml = unaryFunc (toGVal . unsafeRawHtml . asText)

        gfnUrlEncode :: Function (Run m)
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

        gfnDefault :: Function (Run m)
        gfnDefault [] = return def
        gfnDefault ((_, x):xs)
            | asBoolean x = return x
            | otherwise = gfnDefault xs

        gfnEquals :: Function (Run m)
        gfnEquals [] = return $ toGVal True
        gfnEquals (x:[]) = return $ toGVal True
        gfnEquals (x:xs) =
            return . toGVal $ Prelude.all ((snd x `looseEquals`) . snd) xs

        looseEquals :: GVal (Run m) -> GVal (Run m) -> Bool
        looseEquals a b
            | isJust (asFunction a) || isJust (asFunction b) = False
            | isJust (asList a) /= isJust (asList b) = False
            | isJust (asDictItems a) /= isJust (asDictItems b) = False
            -- Both numbers: do numeric comparison
            | isJust (asNumber a) && isJust (asNumber b) = asNumber a == asNumber b
            -- If either is NULL, the other must be falsy
            | isNull a || isNull b = asBoolean a == asBoolean b
            | otherwise = asText a == asText b

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

        gfnCenter :: Function (Run m)
        gfnCenter [] = gfnCenter [(Nothing, toGVal ("" :: Text))]
        gfnCenter (x:[]) = gfnCenter [x, (Nothing, toGVal (80 :: Int))]
        gfnCenter (x:y:[]) = gfnCenter [x, y, (Nothing, toGVal (" " :: Text))]
        gfnCenter ((_, s):(_, w):(_, pad):_) =
            return . toGVal $ center (asText s) (fromMaybe 80 $ Prelude.truncate <$> asNumber w) (asText pad)

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
makeContextM :: (Monad m, Functor m) => (VarName -> Run m (GVal (Run m))) -> (Html -> m ()) -> GingerContext m
makeContextM l w = GingerContext l (liftRun2 w)

liftLookup :: (Monad m, ToGVal (Run m) v) => (VarName -> m v) -> VarName -> Run m (GVal (Run m))
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
makeContext :: (VarName -> GVal (Run (Writer Html))) -> GingerContext (Writer Html)
makeContext l =
    makeContextM
        (return . l)
        tell

-- | Purely expand a Ginger template. The underlying carrier monad is 'Writer'
-- 'Html', which is used to collect the output and render it into a 'Html'
-- value.
runGinger :: GingerContext (Writer Html) -> Template -> Html
runGinger context template = execWriter $ runGingerT context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad.
runGingerT :: (Monad m, Functor m) => GingerContext m -> Template -> m ()
runGingerT context tpl = runReaderT (evalStateT (runTemplate tpl) (defRunState tpl)) context

-- | Internal type alias for our template-runner monad stack.
type Run m = StateT (RunState m) (ReaderT (GingerContext m) m)

-- | Lift a value from the host monad @m@ into the 'Run' monad.
liftRun :: Monad m => m a -> Run m a
liftRun = lift . lift

-- | Lift a function from the host monad @m@ into the 'Run' monad.
liftRun2 :: Monad m => (a -> m b) -> a -> Run m b
liftRun2 f x = liftRun $ f x

-- | Find the effective base template of an inheritance chain
baseTemplate :: Template -> Template
baseTemplate t =
    case templateParent t of
        Nothing -> t
        Just p -> baseTemplate p

-- | Run a template.
runTemplate :: (Monad m, Functor m) => Template -> Run m ()
runTemplate = runStatement . templateBody . baseTemplate

-- | Run an action within a different template context.
withTemplate :: (Monad m, Functor m) => Template -> Run m a -> Run m a
withTemplate tpl a = do
    oldTpl <- gets rsCurrentTemplate
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentTemplate = tpl, rsCurrentBlockName = Nothing })
    result <- a
    modify (\s -> s { rsCurrentTemplate = oldTpl, rsCurrentBlockName = oldBlockName })
    return result

-- | Run an action within a block context
withBlockName :: (Monad m, Functor m) => VarName -> Run m a -> Run m a
withBlockName blockName a = do
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentBlockName = Just blockName })
    result <- a
    modify (\s -> s { rsCurrentBlockName = oldBlockName })
    return result

lookupBlock :: (Monad m, Functor m) => VarName -> Run m Block
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
runStatement :: (Monad m, Functor m) => Statement -> Run m ()
runStatement NullS = return ()
runStatement (MultiS xs) = forM_ xs runStatement
runStatement (LiteralS html) = echo html
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
        runInner :: (Functor m, Monad m) => Run m ()
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
macroToGVal :: forall m. (Functor m, Monad m) => Macro -> GVal (Run m)
macroToGVal (Macro argNames body) =
    fromFunction f
    where
        f :: Function (Run m)
        -- Establish a local state to not contaminate the parent scope
        -- with function arguments and local variables, and;
        -- Establish a local context, where we override the HTML writer,
        -- rewiring it to append any output to the state's capture.
        f args =
            withLocalState . local (\c -> c { contextWriteHtml = appendCapture }) $ do
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
                    matchArgs' :: [(Maybe Text, GVal (Run m))] -> (HashMap Text (GVal (Run m)), [GVal (Run m)], HashMap Text (GVal (Run m)))
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
withLocalScope :: (Monad m) => Run m a -> Run m a
withLocalScope a = do
    scope <- gets rsScope
    r <- a
    modify (\s -> s { rsScope = scope })
    return r

setVar :: Monad m => VarName -> GVal (Run m) -> Run m ()
setVar name val = do
    vars <- gets rsScope
    let vars' = HashMap.insert name val vars
    modify (\s -> s { rsScope = vars' })

getVar :: Monad m => VarName -> Run m (GVal (Run m))
getVar key = do
    vars <- gets rsScope
    case HashMap.lookup key vars of
        Just val ->
            return val
        Nothing -> do
            l <- asks contextLookup
            l key

clearCapture :: Monad m => Run m ()
clearCapture = modify (\s -> s { rsCapture = unsafeRawHtml "" })

appendCapture :: Monad m => Html -> Run m ()
appendCapture h = modify (\s -> s { rsCapture = rsCapture s <> h })

fetchCapture :: Monad m => Run m Html
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
echo :: (Monad m, Functor m, ToHtml h) => h -> Run m ()
echo src = do
    p <- asks contextWriteHtml
    p (toHtml src)
