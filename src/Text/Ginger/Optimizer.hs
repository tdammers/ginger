{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts #-}
-- | A syntax tree optimizer
module Text.Ginger.Optimizer
( Optimizable (..) )
where

import Text.Ginger.AST
import Text.Ginger.GVal
import Text.Ginger.Run
import Control.Monad.Identity
import Data.Default
import Control.Monad.State (execState, evalState)
import Control.Monad.Writer (Writer, execWriter, tell)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.Semigroup as Semigroup

class Optimizable a where
    optimize :: a -> a

instance Optimizable (Template a) where
    optimize = optimizeTemplate

instance Optimizable (Statement a) where
    optimize = optimizeStatement

instance Optimizable (Block a) where
    optimize = optimizeBlock

instance Optimizable (Macro a) where
    optimize = optimizeMacro

instance Optimizable (Expression a) where
    optimize = optimizeExpression

optimizeTemplate t =
    t { templateBody = optimize $ templateBody t
      , templateBlocks = optimize <$> templateBlocks t
      , templateParent = optimize <$> templateParent t
      }

--    = MultiS p [Statement a] -- ^ A sequence of multiple statements
--    | ScopedS p Statement a -- ^ Run wrapped statement in a local scope
--    | LiteralS p Html -- ^ Literal output (anything outside of any tag)
--    | InterpolationS p Expression a -- ^ {{ expression }}
--    | IfS p Expression a Statement a Statement a -- ^ {% if expression %}statement{% else %}statement{% endif %}
--    | ForS p (Maybe VarName) VarName Expression a Statement a -- ^ {% for index, varname in expression %}statement{% endfor %}
--    | SetVarS p VarName Expression a -- ^ {% set varname = expr %}
--    | DefMacroS p VarName Macro a -- ^ {% macro varname %}statements{% endmacro %}
--    | BlockRefS p VarName
--    | PreprocessedIncludeS p Template a -- ^ {% include "template" %}
--    | NullS p -- ^ The do-nothing statement (NOP)

optimizeStatement (MultiS p items) =
    case optimizeStatementList items of
        [] -> NullS p
        [x] -> x
        xs -> MultiS p xs
optimizeStatement (InterpolationS p e) =
    InterpolationS p (optimize e)
optimizeStatement s@(IfS p c t f) =
    let c' = optimize c
        t' = optimize t
        f' = optimize f
    in case compileTimeEval c' of
        Just gv -> case asBoolean gv of
            True -> t
            False -> f
        _ -> s
optimizeStatement s = s

optimizeBlock (Block b) = Block $ optimize b

optimizeMacro (Macro args body) = Macro args (optimize body)

optimizeStatementList =
    mergeLiterals .
    cullNulls .
    fmap optimize

cullNulls :: [Statement a] -> [Statement a]
cullNulls = filter (not . isNullS)
    where
        isNullS (NullS _) = True
        isNullS _ = False

mergeLiterals :: [Statement a] -> [Statement a]
mergeLiterals [] = []
mergeLiterals [x] = [x]
mergeLiterals (x@(LiteralS p1 a):y@(LiteralS p2 b):xs) = mergeLiterals $ (LiteralS p1 $ a <> b):xs
mergeLiterals (x:xs) = x:mergeLiterals xs


-- data Expression a
--     = StringLiteralE p Text -- ^ String literal expression: "foobar"
--     | NumberLiteralE p Scientific -- ^ Numeric literal expression: 123.4
--     | BoolLiteralE p Bool -- ^ Boolean literal expression: true
--     | NullLiteralE p -- ^ Literal null
--     | VarE p VarName -- ^ Variable reference: foobar
--     | ListE p [Expression a] -- ^ List construct: [ expr, expr, expr ]
--     | ObjectE p [(Expression a, Expression a)] -- ^ Object construct: { expr: expr, expr: expr, ... }
--     | MemberLookupE p Expression a Expression a -- ^ foo[bar] (also dot access)
--     | CallE p Expression a [(Maybe Text, Expression a)] -- ^ foo(bar=baz, quux)
--     | LambdaE p [Text] Expression a -- ^ (foo, bar) -> expr
--     | TernaryE p Expression a Expression a Expression a -- ^ expr ? expr : expr
--     deriving (Show)

data Purity = Pure | Impure
    deriving (Show, Eq, Enum, Read, Ord, Bounded)

bothPure :: Purity -> Purity -> Purity
bothPure Pure Pure = Pure
bothPure _ _ = Impure

instance Semigroup.Semigroup Purity where
    (<>) = bothPure

instance Monoid Purity where
    mempty = Pure
    mappend = (<>)

pureExpression :: Expression a -> Purity
pureExpression (StringLiteralE p _) = Pure
pureExpression (NumberLiteralE p _) = Pure
pureExpression (NullLiteralE p) = Pure
pureExpression (ListE p items) = mconcat . map pureExpression $ items
pureExpression (ObjectE p pairs) =
    mconcat [ bothPure (pureExpression k) (pureExpression v)
            | (k, v) <- pairs
            ]
pureExpression (LambdaE _ args body) = pureExpression body
pureExpression (TernaryE _ cond yes no) =
    pureExpression cond <> pureExpression yes <> pureExpression no
pureExpression (MemberLookupE _ k v) =
    pureExpression k <> pureExpression v
pureExpression (CallE _ (VarE _ name) args) =
    pureFunction name <> mconcat (map (pureExpression . snd) args)
pureExpression _ = Impure

pureFunction name
    | name `elem` pureFunctionNames = Pure
    | otherwise = Impure

pureFunctionNames =
    [ "raw"
    , "abs"
    , "any"
    , "all"
    , "capitalize"
    , "ceil"
    , "center"
    , "concat"
    , "contains"
    , "default"
    , "dictsort"
    , "difference"
    , "e"
    , "equals"
    , "escape"
    , "filesizeformat"
    , "filter"
    , "floor"
    , "format"
    , "greater"
    , "greaterEquals"
    , "int"
    , "int_ratio"
    , "iterable"
    , "length"
    , "less"
    , "lessEquals"
    , "modulo"
    , "nequals"
    , "num"
    , "product"
    , "ratio"
    , "replace"
    , "round"
    , "show"
    , "slice"
    , "sort"
    , "str"
    , "sum"
    , "truncate"
    , "urlencode"
    ]

optimizeExpression :: Expression a -> Expression a
optimizeExpression = preEvalExpression . expandConstExpressions . optimizeSubexpressions

preEvalExpression :: Expression a -> Expression a
preEvalExpression e = fromMaybe e $ do
    compileTimeEval e >>= gvalToExpression (annotation e)

gvalToExpression :: forall a m
                  . a -> GVal m -> Maybe (Expression a)
gvalToExpression p g =
    (jsonLiteral =<< asJSON g) <|>
    (ObjectE p <$> (recurseDict =<< asDictItems g)) <|>
    (ListE p <$> (mapM (gvalToExpression p) =<< asList g))
    where
        jsonLiteral :: JSON.Value -> Maybe (Expression a)
        jsonLiteral (JSON.Bool b) = Just (BoolLiteralE p b)
        jsonLiteral (JSON.String s) = Just (StringLiteralE p s)
        jsonLiteral (JSON.Null) = Just (NullLiteralE p)
        jsonLiteral (JSON.Number n) = Just (NumberLiteralE p n)
        jsonLiteral _ = Nothing
        recurseDict :: [(Text, GVal m)] -> Maybe [(Expression a, Expression a)]
        recurseDict = mapM $ \(key, val) -> do
            let key' = StringLiteralE p key
            val' <- gvalToExpression p val
            return (key', val')


expandConstExpressions :: Expression a -> Expression a
expandConstExpressions e@(TernaryE p c t f) =
    case compileTimeEval c of
        Just gv -> case asBoolean gv of
            True -> optimizeExpression t
            False -> optimizeExpression f
        _ -> e
expandConstExpressions e = e

optimizeSubexpressions (ListE p xs) = ListE p (map optimize xs)
optimizeSubexpressions (ObjectE p xs) = ObjectE p [ (optimize k, optimize v) | (k, v) <- xs ]
optimizeSubexpressions (MemberLookupE p k m) = MemberLookupE p (optimize k) (optimize m)
optimizeSubexpressions (CallE p f args) = CallE p (optimize f) [(n, optimize v) | (n, v) <- args]
optimizeSubexpressions (LambdaE p args body) = LambdaE p args (optimize body)
optimizeSubexpressions (TernaryE p c t f) = TernaryE p (optimize c) (optimize t) (optimize f)
optimizeSubexpressions e = e

isConstExpression :: Expression a -> Bool
isConstExpression (StringLiteralE p _) = True
isConstExpression (BoolLiteralE p _) = True
isConstExpression (NullLiteralE p) = True
isConstExpression (ListE p xs) = all isConstExpression xs
isConstExpression (ObjectE p xs) = all (\(k,v) -> isConstExpression k && isConstExpression v) xs
isConstExpression (MemberLookupE p k m) = isConstExpression k && isConstExpression m
isConstExpression e = False

compileTimeEval :: Expression p -> Maybe (GVal Identity)
compileTimeEval (StringLiteralE p s) = Just . toGVal $ s
compileTimeEval (NumberLiteralE p n) = Just . toGVal $ n
compileTimeEval (BoolLiteralE p b) = Just . toGVal $ b
compileTimeEval (NullLiteralE p) = Just def
compileTimeEval e = case pureExpression e of
    Pure -> do
        let tpl =
              -- We're erasing source code positions here,
              -- because we don't have any use for them anyway.
              Template
                  (InterpolationS () (fmap (const ()) e))
                  HashMap.empty
                  Nothing
        Just . toGVal . runCT $ tpl
    Impure -> Nothing

newtype Collected = Collected [GVal Identity]
    deriving (Semigroup.Semigroup, Monoid)

instance ToGVal m Collected where
    toGVal = collectedToGVal

collectedToGVal :: Collected -> GVal m
collectedToGVal (Collected []) = def
collectedToGVal (Collected (x:_)) = marshalGVal x

runCT :: Template () -> Collected
runCT = runGinger ctContext

ctContext :: GingerContext () (Writer Collected) Collected
ctContext = makeContext' ctLookup ctEncode Nothing

ctLookup :: VarName -> GVal m
ctLookup = const def

ctEncode :: GVal m -> Collected
ctEncode g = Collected [marshalGVal g]
