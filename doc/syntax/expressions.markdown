% Expressions

Variables aren't the only thing that can go inside `{{ ... }}`; any valid
Ginger expression can be used, and expressions can be constructed in many
different ways. Note that all expressions are case sensitive: `null` and
`Null` are not the same thing.
Currently, the following constructs are available:

# Simple ("Atomic") Expressions
## Variable reference
Look up a variable in the current scope and return its value.

    {{ username }}

## String literals
A constant string. Strings can be single- or double-quoted.

    {{ "Hello, world!" }}

## Numeric literals
Numeric literals can be given in integer or decimal format:

    {{ 21 }}
    {{ 44.5 }}

## Boolean literals
There are two boolean values, `true` and `false`. You will not normally
need to use them, as they are produced by boolean expressions such as
comparisons, but they can be useful occasionally.

    {{ true }}
    {{ false }}

## The Null literal
Represents the special *null* value, which is used to signal the absence
of a result or value. Looking up non-existent scope variables, for example,
will produce Null.

    {{ null }}

# Data Structures
## List Literals
A list literal consists of a comma-separated list of expressions between
square brackets:

    {{ [ foo, "bar", 1234 ] }}

Lists can contain any kind of expression, including other lists, so you can
nest them:

    {{ [ foo, [ 1, 2, 3 ], "baz" ] }}

## Object Literals
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

# Parentheses
Parentheses can be used to group expression constructs to override default
precedence. Excess parentheses are simply ignored.

# Unary operators
## Function calls
When an expression evaluates to a function, you can call it using a list
of arguments between parentheses. Most of the time, the function itself just
comes from a scope variable, so a function call typically looks like this:

    {{ print(x, "hello!") }}

However, anything that returns a function can be called as a function:

    {{ system.console['log']("hello!") }}

## Filter expressions
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

# Binary operators

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

# The Ternary Operator

Ginger borrows a ternary operator from the C family of languages. The syntax
is:

    condition ? yes-expression : no-expression

If condition evaluates to a truthy value, then the yes-expression is returned,
otherwise, the no-expression is returned.

The ternary operator has lower precedence than any binary operator, and it
associates such that `a ? b ? c : d : e` is equivalent to
`a ? (b ? c : d) : e`.

As an alternative syntax flavor, Ginger also supports a Python-style ternary
construct: `b if a else c` is equivalent to `a ? b : c`.

# Lambda expressions

Lambda expressions allow you to create functions on the fly. The syntax is:

    (argName0, argName1, ... argNameN) -> expression

This creates a function value which, when called, binds arguments to the
argument names `argName0` through `argNameN`, and evaluates `expression` in
that context.

# Do expressions

A `do` expression lifts statements into expressions, that it, it allows you to
execute a statement (or a block of statements) and use its return value as the
value of the expression. Inside the `do` block, statements are written in
[Script Mode Syntax](script.html).

    {% set result = do { echo "hello"; "Said hello"; } %}

This will first print "hello", and then return the string "Said hello", which
is then assigned to the variable `result`.
