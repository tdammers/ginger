% Statements

All statements use `{% %}` delimiters. The `{%-` and `-%}` variations remove
all whitespace (including line breaks) to the left and right respectively.

# `if`

The `{% if %}` statement evaluates an expression and branches based on its
truthiness.

## Simple form:

```ginger
{% if condition %}
True branch body
{% endif %}
```

## With else branch:

```ginger
{% if condition %}
True branch body
{% else %}
False branch body
{% endif %}
```

## With else-if branches:

```ginger
{% if condition1 %}
True branch body
{% elif condition2 %}
Other branch body
{% else %}
False branch body
{% endif %}
```

# `switch`

For branching into multiple branches based on one expression, use `{% switch
%}`:

```ginger
{% switch expression %}
  {% case value1 %}
    Value 1 body
  {% endcase %}
  {% case value2 %}
    Value 2 body
  {% endcase %}
  {% default %}
    Default body
  {% enddefault %}
{% endswitch %}
```

# `set`

`{% set varname = expression %}` defines a variable and sets it to the value of
the `expression`.

# `include`

`{% include "filename" %}` reads a file and injects it into the template
exactly where the `include` is written.

# `scope`

`{% scope %}...{% endscope %}` creates an explicit local scope:

- variables defined inside the scope disappear when the scope is left
- variables defined outside the scope are available inside it
- variables defined outside the scope and again inside it will take on the
  value from inside the scope while inside, but revert to the value from
  outside after leaving the scope

# `macro`

`{% macro name(arg0,...) %}...{% endmacro %}` defines a macro named `name`.
Once defined, the macro can be called like a function or filter. Alternatively,
macros can be called using the `{% call %}` construct, see below.

# `call`

`{% call (arg0) name(arg1) %}body{% endcall %}` calls the macro named `name`
with argument `arg1`; inside the macro's body, a special function named
`caller` is available that takes an argument which will then be passed back
into the body of the `call` statement. If this makes your brain hurt, read the
example in our simulation tests, reproduced here for convenience:

Template:

```ginger
{% macro foobar3(a) -%}
  {{ a }}({{ caller("asdf") }})
{%- endmacro -%}

{%- call (a) foobar3("hey") -%}
<{{a}}>
{%- endcall %}
```

Output:

```html
hey(<asdf>)
```

# `extends`
# `block`

These two together implement *template inheritance*. Here's how it works:

- First, you write a parent template, which is really just a regular template.
  However, some parts of it are enclosed in `{% block blockname %}` ... `{%
  endblock %}`
- Then you write a child template, which starts with an `{% include
  'parent-template-name' %}` statement, followed by zero or more blocks. These
  blocks override the content of the blocks of the same name in the parent
  template.
- Blocks can be nested; in that case, the child template can choose to override
  the outer block (thereby removing the inner block entirely), or override the
  inner block (leaving the rest of the containing block unchanged).
- Blocks that are not overridden, as well as content outside of any blocks, is
  copied from the parent template.
