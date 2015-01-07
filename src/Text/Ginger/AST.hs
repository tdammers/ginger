-- | Implements Ginger's Abstract Syntax Tree.
module Text.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Ginger.Html

-- | A context variable name.
type VarName = Text

-- | Top-level data structure, representing a fully parsed template.
data Template
    = Template
        { templateBody :: Statement
        }
        deriving (Show)

-- | Ginger statements.
data Statement
    = MultiS [Statement] -- ^ A sequence of multiple statements
    | LiteralS Html -- ^ Literal output (anything outside of any tag)
    | InterpolationS Expression -- ^ {{ expression }}
    | IfS Expression Statement Statement -- ^ {% if expression %}statement{% else %}statement{% endif %}
    | ForS (Maybe VarName) VarName Expression Statement -- ^ {% for index, varname in expression %}statement{% endfor %}
    | NullS -- ^ The do-nothing statement (NOP)
    deriving (Show)

-- | Expressions, building blocks for the expression minilanguage.
data Expression
    = StringLiteralE Text -- ^ String literal expression: "foobar"
    | VarE VarName -- ^ Variable reference: foobar
    deriving (Show)
