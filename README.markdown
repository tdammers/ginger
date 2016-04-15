# Ginger

A note of warning: the git repository at https://bitbucket.org/tdammers/ginger
has been deleted and restored with a rewritten commit tree on 2016-04-06 in
order to clean up the messy history. This means that if you have a checkout
from before that date, merging the bitbucket repo will most likely break
things; please do a fresh clone to fix this. Sorry for the inconvenience.

## Intro

A Haskell implementation of the <http://jinja.pocoo.org/ Jinja2> template
language.

Ginger aims to be as close to the original Jinja language as possible, but
avoiding blatant pythonisms and features that make little sense outside of
an impure dynamic host language context, especially when this would require
sacrificing runtime performance.

## Template Syntax

### Minimal example template

    <!DOCTYPE html>
    <html>
        <head>
            <title>{{ title }}</title>
        </head>
        {# This is a comment. Comments are removed from the output. #}
        <body>
            <menu id="nav-main">
            {% for item in navigation %}
                <li><a href="{{ item.url }}">{{ item.label }}</a></li>
            {% endfor %}
            </menu>
            <div class="layout-content-main">
                <h1>{{ title }}</h1>
                {{ body }}
            </div>
        </body>
    </html>

There are two kinds of delimiters. `{% ... %}` and `{{ ... }}`. The first
one is used to execute statements such as for-loops or assign values, the
latter prints the result of an expression to the template.

*Not implemented yet*: Jinja2 allows the programmer to override the default
tags from `{% %}` and `{{ }}` to different tokens, e.g. `<% %>` and `<< >>`.
Ginger does not currently support this.

### Variables
You can mess around with the variables in templates provided they are
passed in by the application. Variables may have attributes or elements
on them you can access too. What attributes a variable has depends
heavily on the application providing that variable.

You can use a dot (`.`) to access attributes of a variable, but
alternatively the so-called “subscript” syntax (`[]`) can be used. The
following lines do the same thing:

    {{ foo.bar }}
    {{ foo['bar'] }}

It’s important to know that the curly braces are *not* part of the
variable, but the print statement. If you access variables inside tags
don’t put the braces around them.

If a variable or attribute does not exist you will get back an undefined
value. What you can do with that kind of value depends on the
application configuration: the default behavior is that it evaluates to
an empty string if printed and that you can iterate over it, but every
other operation fails.

### Expressions
Variables aren't the only thing that can go inside `{{ ... }}`; any valid
Ginger expression can be used, and expressions can be constructed in many
different ways. Note that all expressions are case sensitive: 1null1 and
1Null1 are not the same thing.
Currently, the following constructs are available:

#### Simple ("Atomic") Expressions
##### Variable reference
Look up a variable in the current scope and return its value.

    {{ username }}

##### String literals
A constant string. Strings can be single- or double-quoted.

    {{ "Hello, world!" }}

##### Numeric literals
Numeric literals can be given in integer or decimal format:

    {{ 21 }}
    {{ 44.5 }}

##### Boolean literals
There are two boolean values, `true` and `false`. You will not normally
need to use them, as they are produced by boolean expressions such as
comparisons, but they can be useful occasionally.

    {{ true }}
    {{ false }}

##### The Null literal
Represents the special *null* value, which is used to signal the absence
of a result or value. Looking up non-existent scope variables, for example,
will produce Null.

    {{ null }}

#### Data Structures
##### List Literals
A list literal consists of a comma-separated list of expressions between
square brackets:

    {{ [ foo, "bar", 1234 ] }}

Lists can contain any kind of expression, including other lists, so you can
nest them:

    {{ [ foo, [ 1, 2, 3 ], "baz" ] }}

##### Object Literals
An object is an unsorted key/value container, declared like this:

    {{ { "foo": "bar", "baz": "quux" } }}

Objects, like lists, can contain any kind of value. The keys, however, are
restricted to strings; you can define them using any expression you like,
but they are always converted to strings when inserting items into the
container. This means that the following is valid:

    {{ { ["foo", 1]: "bar" } }}

It will, however, produce the same object as the following:

    {{ { "foo1": "bar" } }}

That's because converting `["foo", 1]` to a string produces `"foo1"`, and
that is used as the key.

#### Parentheses
Parentheses can be used to group expression constructs to override default
precedence. Excess parentheses are simply ignored.

#### Unary operators
##### Function calls
When an expression evaluates to a function, you can call it using a list
of arguments between parentheses. Most of the time, the function itself just
comes from a scope variable, so a function call typically looks like this:

    {{ print(x, "hello!") }}

However, anything that returns a function can be called as a function:

    {{ system.console['log']("hello!") }}

##### Filter expressions
Any function that takes at least one argument can be called through the
alternative filter syntax instead:

    {{ x|print }}

This is equivalent to:

    {{ print(x) }}

Filters can take arguments:

    {{ x|append("foobar") }}

This is equivalent to:

    {{ append(x, "foobar") }}

*Deviation from Jinja2:* Ginger does not distinguish between filters and
functions at the semantics level; any function can be called as a filter,
and vv., and the filter syntax is merely a syntactic variant of a function
call. Further, Ginger treats functions as first-class values, and their names
are just regular variable names.

#### Binary operators

All binary operators have aliases that are regular functions; they are listed
below. For example, `1 + 2` resolves to `sum(1, 2)`. The aliases are variadic,
that is, you can pass more than two arguments, e.g. `sum(1, 2, 4, 5)`.  This
makes them slightly more useful than the operators, at the expense of
readability.

In order of precedence:

1. Boolean (logical) operators:
   - `||` (logical OR). Alias: `any`
   - `&&` (logical AND). Alias: `all`
2. Comparison operators:
   - `==` (loose equality). Alias: `equals`
   - `!=` (loose inequality). Alias: `nequals`
   - `<` (numeric less-than). Alias: `less`
   - `<=` (numeric less-than-or-equal). Alias: `lessEquals`
   - `>` (numeric greater-than). Alias: `greater`
   - `>=` (numeric greater-than-or-equal). Alias: `greaterEquals`
3. Additive operators:
   - `+` (addition). Alias: `sum`
   - `-` (subtraction). Alias: `difference`
   - `~` (string concatenation). Alias: `concat`
4. Multiplicative operators:
   - `*` (multiplication). Alias: `product`
   - `//` (integer division). Alias: `int_ratio`
   - `/` (division). Alias: `ratio`
   - `%` (modulo). Alias: `modulo`

#### The Ternary Operator

Ginger borrows a ternary operator from the C family of languages. The syntax
is:

    condition ? yes-expression : no-expression

If condition evaluates to a truthy value, then the yes-expression is returned,
otherwise, the no-expression is returned.

The ternary operator has lower precedence than any binary operator, and it
associates such that `a ? b ? c : d : e` is equivalent to
`a ? (b ? c : d) : e`.

#### Lambda expressions

Lambda expressions allow you to create functions on the fly. The syntax is:

    (argName0, argName1, ... argNameN) -> expression

This creates a function value which, when called, binds arguments to the
argument names `argName0` through `argNameN`, and evaluates `expression` in
that context.

## Haskell API
### General
On the Haskell side of things, executing a template is a two-step process.
First, template source code is parsed into a 'Template' data structure,
which is then fed to 'runGinger' or 'runGingerT'.

###  Parsing
Because Ginger templates can include other templates, the parser needs a way of
resolving template names. Instead of hard-wiring the parser into 'IO' though,
Ginger will work on any Monad type, but requires the caller to provide a
suitable template resolver function. For 'IO', the resolver would typically
load a file from a template directory, but other monads might have access to
some sort of cache, or expose template compiled into a program, or simply
return 'Nothing' unconditionally to disable any and all imports. A suitable
example implementation for 'IO' would look like this:

    loadFile fn = openFile fn ReadMode >>= hGetContents

    loadFileMay fn =
        tryIOError (loadFile fn) >>= \e ->
             case e of
                Right contents ->
                    return (Just contents)
                Left err -> do
                    print err -- remove this line if you want to fail silently
                    return Nothing

(Taken from `cli/GingerCLI.hs`). This interprets the template name as a
filename relative to the CWD, and returns the file contents on success or
'Nothing' if there is any error.

If you don't need a monadic context for resolving includes (e.g. because you
have pre-loaded all template sources), you can use the pure 'parseGinger'
flavor, which does not rely on a host monad.

### Running
The core function for running a template is 'runGinger' (or its monadic
flavor 'runGingerT'); in order to pass an initial context to the template
engine, pass a suitable 'GingerContext', which you can create using the
'makeContext' / 'makeContextM' functions.

An example call (for running a template in 'IO') would look something like
this:

    runGingerT (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) tpl
