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
data Template
    = Template
        { templateBody :: Statement
        , templateBlocks :: HashMap VarName Block
        , templateParent :: Maybe Template
        }
        deriving (Show)

-- | A macro definition ( @{% macro %}@ )
data Macro
    = Macro { macroArgs :: [VarName], macroBody :: Statement }
    deriving (Show)

-- | A block definition ( @{% block %}@ )
data Block
    = Block { blockBody :: Statement } -- TODO: scoped blocks
    deriving (Show)

-- | Ginger statements.
data Statement
    = MultiS [Statement] -- ^ A sequence of multiple statements
    | ScopedS Statement -- ^ Run wrapped statement in a local scope
    | LiteralS Html -- ^ Literal output (anything outside of any tag)
    | InterpolationS Expression -- ^ {{ expression }}
    | ExpressionS Expression -- ^ Evaluate expression
    | IfS Expression Statement Statement -- ^ {% if expression %}statement{% else %}statement{% endif %}
    | SwitchS Expression [(Expression, Statement)] Statement -- ^ {% switch expression %}{% case expression %}statement{% endcase %}...{% default %}statement{% enddefault %}{% endswitch %}
    | ForS (Maybe VarName) VarName Expression Statement -- ^ {% for index, varname in expression %}statement{% endfor %}
    | SetVarS VarName Expression -- ^ {% set varname = expr %}
    | DefMacroS VarName Macro -- ^ {% macro varname %}statements{% endmacro %}
    | BlockRefS VarName
    | PreprocessedIncludeS Template -- ^ {% include "template" %}
    | NullS -- ^ The do-nothing statement (NOP)
    deriving (Show)

-- | Expressions, building blocks for the expression minilanguage.
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
    | DoE Statement -- ^ do { statement; }
    deriving (Show)
