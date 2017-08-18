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

# `indent`

`{% indent %}...{% endindent %}` creates an indentation scope using the default
indent (two spaces)

`{% indent expr %}...{% endindent %}` interprets `expr` as an indent and
creates a matching indentation scope.

Indentation blocks can be nested.

Existing indentation in the template source is removed inside `{% indent %}`
blocks, based on the indentation of the first line after the opening
`{% indent %}` tag. After that first line, all subsequent lines that begin with
the same indent will have it stripped, while any additional whitespace will be
kept, as will whitespace sequences that differ from the first line.

After that, indentation blocks add one level of indentation, except for the
outermost one, which serves just to establish the indentation context in the
first place.

Indentation blocks cover all line endings in the output, regardless of whether
they come from literal output (bare text), interpolations, macro invocations,
or includes.

Indentation level is determined from the runtime context, so if you put an
indentation block inside a macro and call it from another indentation block,
the two will nest as if you had written the macro body directly into the
calling context. This means that the following:

```ginger
{%- macro foobar %}
{% indent '' %}
<div>
{% indent '  ' %}
<h1>Hello!</h1>
{% endindent %}
</div>
{% endindent %}
{% endmacro -%}


{% indent %}
<body>
{{ foobar() }}
</body>
{% endindent }
```

...will render as:

```html
<body>
  <div>
    <h1>Hello!</h1>
  </div>
</body>
```

Thus, `{% indent %}` allows authors to write reusable template code that
produces correct indentation.

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

# `try`
# `catch`
# `finally`

Exception handling. These work similar to exception handling constructs in
other languages. Example:

```ginger
{% try %}
  <div>
  {# This block gets executed unconditionally, until an exception is thrown. #}
  {{ something_that_may_fail() }}
{% catch 'ArgumentsError' as exception %}
  {# This block fires when the 'try' block throws an exception of type
     'ArgumentsError'. #}
  <p class="warning">Invalid arguments.</p>
{% catch * as exception %}
  {# This block fires on any other exception. #}
  <p class="error">Something went wrong.</p>
{% finally %}
  </div>
{% endtry %}
```

The way error handling flow works is as follows:

- A try/catch/finally block is enclosed in `{% try %}`...`{% endtry %}`.
- Everything up to the first `{% catch %}` or `{% finally %}` is executed,
  until an exception is thrown.
- Upon throwing an exception, ginger proceeds to checking the available `catch`
  blocks, in the order in which they are written in the source file.
- As soon as one of the `catch` blocks matches, ginger enters it, runs it, and
  skips the remaining `catch` blocks after it.
- If none of the `catch` blocks match, the exception is kept alive and gets
  re-thrown at the end of the `try` construct.
- At this point, four possible situations can exist: a) no exception has
  occurred; b) an exception has occurred and has been caught; c) an exception
  has occurred and hasn't been caught; d) an exception has been caught, but a
  new exception has been thrown while handling the original one. In all four
  cases, ginger will first execute the `finally` block; after that, it will
  "bubble" any remaining uncaught exceptions (cases c and d), or return without
  error (cases a and b).

## `catch` Statements

The `catch` statement header comes in multiple flavors. The most complete one
is:

```ginger
{% catch what as name %}
```

Where:

- `what` is either the special token `*` to catch any exception, or a string
  literal that must match the exception type.
- `name` is a valid identifier name; the caught exception will be bound to that
  name for the duration of the catch block, which is useful if you want to
  inspect the exception further.

Both the `what` filter and the `name` are optional, however, if you want to
bind to a `name`, you also need to provide a `what` (use `*` to catch
everything). So the following forms are all valid:

```ginger
{% catch %}
  {# anonymously catch any exception #}
```

```ginger
{% catch * %}
  {# anonymously catch any exception #}
```

```ginger
{% catch 'ArgumentsError' %}
  {# anonymously catch only ArgumentsErrors #}
```

```ginger
{% catch * as bloop %}
  {# catch any exception, and bind it to 'bloop' #}
```

```ginger
{% catch 'ArgumentsError' as bloop %}
  {# catch 'ArgumentsError' exception, and bind it to 'bloop' #}
```

## Exception Objects

Exception objects are regular Ginger values; when used as a dictionary, they
expose at least the following common properties:

- `what` - A string containing the exact exception type. See below for a list
  of possible exception types.
- `message` - A human-readable error message.

## Exception Types

### `RuntimeError`

A generic run-time error.

#### Properties:

- `what`: Exception type
- `message`: Human-readable error message

### `UndefinedBlockError`

Thrown when a template attempts to explicitly call a block that hasn't been
defined yet.

#### Properties:

- `what`: Exception type
- `message`: Human-readable error message
- `block`: Name of the non-existent block

### `ArgumentsError`

A function call has received arguments that do not match its accepted
arguments.

#### Properties:

- `what`: Exception type
- `message`: Human-readable error message
- `function`: The canonical name of the function that was called
- `explanation`: Explanation how exactly the arguments failed to meet the
  function's expectations

### `EvalParseError`

Thrown when code passed to `eval` dynamically contains a syntax error.

#### Properties:

- `what`: Exception type
- `message`: Human-readable error message
- `errorMessage`: Error message from the Ginger parser
- `sourceFile`: Source file name, if any. Typically not useful.
- `line`: Line number of the parser error. This is relative to the evaluated
  string, not the source file the `eval` call was made from.
- `col`: Column number of the parser error. Just like `line`, this refers to
  the evaluated string, not the originating source file.

### `NotAFunctionError`

Thrown when trying to call a non-function as a function, or using a
non-function as a filter.

#### Properties:

- `what`: Exception type
- `message`: Human-readable error message

# `script`

The `{% script %}` statement introduces a Script Mode Block. Inside a script
block, Ginger uses an alternate syntax that is more similar to an imperative
programming language like C or JavaScript, and less similar to a regular
template language. Most of the statements are still available, but the syntax
is different: for example, `{% for i in items %}{{ item }}{% endfor %}` becomes
`for (i in items) { echo(item); }`. For full details, see [Script
Mode](script.html).
