module Text.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text

type VarName = Text

data Template
    = Template
        { templateBody :: Statement
        }

data Statement
    = MultiS [Statement] -- ^ A sequence of multiple statements
    | LiteralS Text -- ^ Literal output
    | InterpolationS Expression -- ^ {{ expression }}
    | IfE Expression Statement Statement -- ^ {% if expression %}statement{% else %}statement{% endif %}
    | ForE VarName Expression Statement -- ^ {% for varname in expression %}statement{% endfor %}

data Expression
    = StringLiteralE Text -- ^ String literal expression: "foobar"
    | VarE Text -- ^ Variable reference: foobar
