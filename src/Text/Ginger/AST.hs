-- | Implements Ginger's Abstract Syntax Tree.
module Text.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text

-- | A context variable name.
type VarName = Text

-- | Top-level data structure, representing a fully parsed template.
data Template
    = Template
        { templateBody :: Statement
        }

-- | Ginger statements.
data Statement
    = MultiS [Statement] -- ^ A sequence of multiple statements
    | LiteralS Text -- ^ Literal output (anything outside of any tag)
    | InterpolationS Expression -- ^ {{ expression }}
    | IfE Expression Statement Statement -- ^ {% if expression %}statement{% else %}statement{% endif %}
    | ForE VarName Expression Statement -- ^ {% for varname in expression %}statement{% endfor %}

-- | Expressions, building blocks for the expression minilanguage.
data Expression
    = StringLiteralE Text -- ^ String literal expression: "foobar"
    | VarE Text -- ^ Variable reference: foobar
