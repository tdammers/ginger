-- | A syntax tree optimizer
module Text.Ginger.Optimizer
( Optimizable (..) )
where

import Text.Ginger.AST
import Text.Ginger.GVal
import Data.Monoid
import Control.Monad.Identity
import Data.Default

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

optimizeExpression :: Expression -> Expression
optimizeExpression = expandConstExpressions . optimizeSubexpressions

expandConstExpressions :: Expression -> Expression
expandConstExpressions e@(TernaryE c t f) =
    case compileTimeEval c of
        Just gv -> case asBoolean gv of
            True -> t
            False -> f
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
compileTimeEval e = Nothing
