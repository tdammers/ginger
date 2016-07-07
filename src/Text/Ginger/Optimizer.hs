-- | A syntax tree optimizer
module Text.Ginger.Optimizer
( Optimizable (..) )
where

import Text.Ginger.AST
import Data.Monoid

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

optimizeTemplate t =
    t { templateBody = optimize $ templateBody t
      , templateBlocks = fmap optimize $ templateBlocks t
      , templateParent = fmap optimize $ templateParent t
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
