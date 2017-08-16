{-#LANGUAGE DeriveFunctor #-}
-- | Implements Ginger's Abstract Syntax Tree.
module Text.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Ginger.Html
import Data.Scientific (Scientific)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


-- | A context variable name.
type VarName = Text

-- | Top-level data structure, representing a fully parsed template.
data Template a
    = Template
        { templateBody :: Statement a
        , templateBlocks :: HashMap VarName (Block a)
        , templateParent :: Maybe (Template a)
        }
        deriving (Show, Functor)

-- | A macro definition ( @{% macro %}@ )
data Macro a
    = Macro { macroArgs :: [VarName], macroBody :: Statement a }
    deriving (Show, Functor)

-- | A block definition ( @{% block %}@ )
data Block a
    = Block { blockBody :: Statement a } -- TODO: scoped blocks
    deriving (Show, Functor)

-- | Ginger statements.
data Statement a
    = MultiS a [Statement a] -- ^ A sequence of multiple statements
    | ScopedS a (Statement a) -- ^ Run wrapped statement in a local scope
    | IndentS a (Expression a) (Statement a) -- ^ Establish an indented context around the wrapped statement
    | LiteralS a Html -- ^ Literal output (anything outside of any tag)
    | InterpolationS a (Expression a) -- ^ {{ expression }}
    | ExpressionS a (Expression a) -- ^ Evaluate expression
    | IfS a (Expression a) (Statement a) (Statement a) -- ^ {% if expression %}statement{% else %}statement{% endif %}
    | SwitchS a (Expression a) [((Expression a), (Statement a))] (Statement a) -- ^ {% switch expression %}{% case expression %}statement{% endcase %}...{% default %}statement{% enddefault %}{% endswitch %}
    | ForS a (Maybe VarName) VarName (Expression a) (Statement a) -- ^ {% for index, varname in expression %}statement{% endfor %}
    | SetVarS a VarName (Expression a) -- ^ {% set varname = expr %}
    | DefMacroS a VarName (Macro a) -- ^ {% macro varname %}statements{% endmacro %}
    | BlockRefS a VarName
    | PreprocessedIncludeS a (Template a) -- ^ {% include "template" %}
    | NullS a -- ^ The do-nothing statement (NOP)
    deriving (Show, Functor)

stmtAnnotation (MultiS a _) = a
stmtAnnotation (ScopedS a _) = a
stmtAnnotation (IndentS a _ _) = a
stmtAnnotation (LiteralS a _) = a
stmtAnnotation (InterpolationS a _) = a
stmtAnnotation (ExpressionS a _) = a
stmtAnnotation (IfS a _ _ _) = a
stmtAnnotation (SwitchS a _ _ _) = a
stmtAnnotation (ForS a _ _ _ _) = a
stmtAnnotation (SetVarS a _ _) = a
stmtAnnotation (DefMacroS a _ _) = a
stmtAnnotation (BlockRefS a _) = a
stmtAnnotation (PreprocessedIncludeS a _) = a
stmtAnnotation (NullS a) = a

-- | Expressions, building blocks for the expression minilanguage.
data Expression a
    = StringLiteralE a Text -- ^ String literal expression: "foobar"
    | NumberLiteralE a Scientific -- ^ Numeric literal expression: 123.4
    | BoolLiteralE a Bool -- ^ Boolean literal expression: true
    | NullLiteralE a -- ^ Literal null
    | VarE a VarName -- ^ Variable reference: foobar
    | ListE a [(Expression a)] -- ^ List construct: [ expr, expr, expr ]
    | ObjectE a [((Expression a), (Expression a))] -- ^ Object construct: { expr: expr, expr: expr, ... }
    | MemberLookupE a (Expression a) (Expression a) -- ^ foo[bar] (also dot access)
    | CallE a (Expression a) [(Maybe Text, (Expression a))] -- ^ foo(bar=baz, quux)
    | LambdaE a [Text] (Expression a) -- ^ (foo, bar) -> expr
    | TernaryE a (Expression a) (Expression a) (Expression a) -- ^ expr ? expr : expr
    | DoE a (Statement a) -- ^ do { statement; }
    deriving (Show, Functor)

exprAnnotation (StringLiteralE a _) = a
exprAnnotation (NumberLiteralE a _) = a
exprAnnotation (BoolLiteralE a _) = a
exprAnnotation (NullLiteralE a) = a
exprAnnotation (VarE a _) = a
exprAnnotation (ListE a _) = a
exprAnnotation (ObjectE a _) = a
exprAnnotation (MemberLookupE a _ _) = a
exprAnnotation (CallE a _ _) = a
exprAnnotation (LambdaE a _ _) = a
exprAnnotation (TernaryE a _ _ _) = a
exprAnnotation (DoE a _) = a

class Annotated f where
    annotation :: f p -> p

instance Annotated Expression where
    annotation = exprAnnotation

instance Annotated Statement where
    annotation = stmtAnnotation

instance Annotated Block where
    annotation = annotation . blockBody

instance Annotated Macro where
    annotation = annotation . macroBody

instance Annotated Template where
    annotation = annotation . templateBody
