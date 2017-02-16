{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
-- | A syntax tree optimizer
module Text.Ginger.Optimizer
( Optimizable (..) )
where

import Text.Ginger.AST
import Text.Ginger.GVal
import Text.Ginger.Run
import Data.Monoid
import Control.Monad.Identity
import Data.Default
import Control.Monad.State (execState, evalState)
import Control.Monad.Writer (Writer, execWriter, tell)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Aeson as JSON

class Optimizable a where
    optimize :: a -> a

instance Optimizable Template where
    optimize = optimizeTemplate

instance Optimizable Statement where
    optimize = optimizeStatement

instance Optimizable Block where
    optimize = optimizeBlock

instance Optimizable Macro where
    optimize = optimizeMacro

instance Optimizable Expression where
    optimize = optimizeExpression

optimizeTemplate t =
    t { templateBody = optimize $ templateBody t
      , templateBlocks = optimize <$> templateBlocks t
      , templateParent = optimize <$> templateParent t
      }

{-
    = MultiS [Statement] -- ^ A sequence of multiple statements
    | ScopedS Statement -- ^ Run wrapped statement in a local scope
    | LiteralS Html -- ^ Literal output (anything outside of any tag)
    | InterpolationS Expression -- ^ {{ expression }}
    | IfS Expression Statement Statement -- ^ {% if expression %}statement{% else %}statement{% endif %}
    | ForS (Maybe VarName) VarName Expression Statement -- ^ {% for index, varname in expression %}statement{% endfor %}
    | SetVarS VarName Expression -- ^ {% set varname = expr %}
    | DefMacroS VarName Macro -- ^ {% macro varname %}statements{% endmacro %}
    | BlockRefS VarName
    | PreprocessedIncludeS Template -- ^ {% include "template" %}
    | NullS -- ^ The do-nothing statement (NOP)
-}
optimizeStatement (MultiS items) =
    case optimizeStatementList items of
        [] -> NullS
        [x] -> x
        xs -> MultiS xs
optimizeStatement (InterpolationS e) =
    InterpolationS (optimize e)
optimizeStatement s@(IfS c t f) =
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

cullNulls :: [Statement] -> [Statement]
cullNulls = filter (not . isNullS)
    where
        isNullS NullS = True
        isNullS _ = False

mergeLiterals :: [Statement] -> [Statement]
mergeLiterals [] = []
mergeLiterals [x] = [x]
mergeLiterals (x@(LiteralS a):y@(LiteralS b):xs) = mergeLiterals $ (LiteralS $ a <> b):xs
mergeLiterals (x:xs) = x:mergeLiterals xs


{-
data Expression
    = StringLiteralE Text -- ^ String literal expression: "foobar"
    | NumberLiteralE Scientific -- ^ Numeric literal expression: 123.4
    | BoolLiteralE Bool -- ^ Boolean literal expression: true
    | NullLiteralE -- ^ Literal null
    | VarE VarName -- ^ Variable reference: foobar
    | ListE [Expression] -- ^ List construct: [ expr, expr, expr ]
    | ObjectE [(Expression, Expression)] -- ^ Object construct: { expr: expr, expr: expr, ... }
    | MemberLookupE Expression Expression -- ^ foo[bar] (also dot access)
    | CallE Expression [(Maybe Text, Expression)] -- ^ foo(bar=baz, quux)
    | LambdaE [Text] Expression -- ^ (foo, bar) -> expr
    | TernaryE Expression Expression Expression -- ^ expr ? expr : expr
    deriving (Show)
-}

data Purity = Pure | Impure
    deriving (Show, Eq, Enum, Read, Ord, Bounded)

bothPure :: Purity -> Purity -> Purity
bothPure Pure Pure = Pure
bothPure _ _ = Impure

instance Monoid Purity where
    mempty = Pure
    mappend = bothPure

pureExpression :: Expression -> Purity
pureExpression (StringLiteralE _) = Pure
pureExpression (NumberLiteralE _) = Pure
pureExpression NullLiteralE = Pure
pureExpression (ListE items) = mconcat . map pureExpression $ items
pureExpression (ObjectE pairs) =
    mconcat [ bothPure (pureExpression k) (pureExpression v)
            | (k, v) <- pairs
            ]
pureExpression (LambdaE args body) = pureExpression body
pureExpression (TernaryE cond yes no) =
    pureExpression cond <> pureExpression yes <> pureExpression no
pureExpression (MemberLookupE k v) =
    pureExpression k <> pureExpression v
pureExpression (CallE (VarE name) args) =
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

optimizeExpression :: Expression -> Expression
optimizeExpression = preEvalExpression . expandConstExpressions . optimizeSubexpressions

preEvalExpression :: Expression -> Expression
preEvalExpression e = fromMaybe e $ do
    compileTimeEval e >>= gvalToExpression

gvalToExpression :: GVal m -> Maybe Expression
gvalToExpression g =
    (jsonLiteral =<< asJSON g) <|>
    (ObjectE <$> (recurseDict =<< asDictItems g)) <|>
    (ListE <$> (mapM gvalToExpression =<< asList g))
    where
        jsonLiteral :: JSON.Value -> Maybe Expression
        jsonLiteral (JSON.Bool b) = Just (BoolLiteralE b)
        jsonLiteral (JSON.String s) = Just (StringLiteralE s)
        jsonLiteral (JSON.Null) = Just NullLiteralE
        jsonLiteral (JSON.Number n) = Just (NumberLiteralE n)
        jsonLiteral _ = Nothing
        recurseDict :: [(Text, GVal m)] -> Maybe [(Expression, Expression)]
        recurseDict = mapM $ \(key, val) -> do
            let key' = StringLiteralE key
            val' <- gvalToExpression val
            return (key', val')


expandConstExpressions :: Expression -> Expression
expandConstExpressions e@(TernaryE c t f) =
    case compileTimeEval c of
        Just gv -> case asBoolean gv of
            True -> optimizeExpression t
            False -> optimizeExpression f
        _ -> e
expandConstExpressions e = e

optimizeSubexpressions (ListE xs) = ListE (map optimize xs)
optimizeSubexpressions (ObjectE xs) = ObjectE [ (optimize k, optimize v) | (k, v) <- xs ]
optimizeSubexpressions (MemberLookupE k m) = MemberLookupE (optimize k) (optimize m)
optimizeSubexpressions (CallE f args) = CallE (optimize f) [(n, optimize v) | (n, v) <- args]
optimizeSubexpressions (LambdaE args body) = LambdaE args (optimize body)
optimizeSubexpressions (TernaryE c t f) = TernaryE (optimize c) (optimize t) (optimize f)
optimizeSubexpressions e = e

isConstExpression :: Expression -> Bool
isConstExpression (StringLiteralE _) = True
isConstExpression (BoolLiteralE _) = True
isConstExpression NullLiteralE = True
isConstExpression (ListE xs) = all isConstExpression xs
isConstExpression (ObjectE xs) = all (\(k,v) -> isConstExpression k && isConstExpression v) xs
isConstExpression (MemberLookupE k m) = isConstExpression k && isConstExpression m
isConstExpression e = False

compileTimeEval :: Expression -> Maybe (GVal Identity)
compileTimeEval (StringLiteralE s) = Just . toGVal $ s
compileTimeEval (NumberLiteralE n) = Just . toGVal $ n
compileTimeEval (BoolLiteralE b) = Just . toGVal $ b
compileTimeEval NullLiteralE = Just def
compileTimeEval e = case pureExpression e of
    Pure -> do
        let tpl = Template (InterpolationS e) HashMap.empty Nothing
        Just . toGVal . runCT $ tpl
    Impure -> Nothing

newtype Collected = Collected [GVal Identity]
    deriving (Monoid)

instance ToGVal m Collected where
    toGVal = collectedToGVal

collectedToGVal :: Collected -> GVal m
collectedToGVal (Collected []) = def
collectedToGVal (Collected (x:_)) = marshalGVal x

runCT :: Template -> Collected
runCT = runGinger ctContext

ctContext :: GingerContext (Writer Collected) Collected
ctContext = makeContext' ctLookup ctEncode Nothing

ctLookup :: VarName -> GVal m
ctLookup = const def

ctEncode :: GVal m -> Collected
ctEncode g = Collected [marshalGVal g]
