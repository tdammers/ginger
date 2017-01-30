% Script Mode

Between `{% script %}` / `{% endscript %}`, Ginger uses an alternate syntax,
making it behave like an imperative scripting language instead of a template
language. Most of the regular statements are available, but their syntax is
different.

# The Basics

A Script Mode block contains a series of zero or more statements. Each
statement ends with a semicolon (`;`), just like in C, C++, Java, etc.
Statements can be grouped into blocks using curly braces (`{` / `}`).

Ginger's control flow statements use the same keywords, but the syntax is
different.

## A Contrived Example

In plain Ginger, you would write:

```ginger
{% for user in users %}
    Username: {{ user.username }}
{% else %}
    No Users Found
{% endfor %}
```

In Script Mode, it would look like this:

```ginger
{% script %}
for (user in users) {
  echo(user.username);
}
else {
  echo("No Users Found");
}
{% endscript %}
```

# Comments

C++ style line comments are allowed at any point where whitespace is allowed:

```ginger
{% script %}
echo( // this is a comment!
  "Hello, world!" // so is this
  // this is a comment, too
) // even this is OK
; // and this, too.
{% endscript %}
```

# Interpolation

Unlike regular Ginger, things written at the top level in Script Mode are
interpreted as statements or expressions, rather than verbatim output. In order
to generate actual output in Script Mode, you need to use the `echo`
pseudo-function.

```ginger
{{ username }}
```

...becomes:

```ginger
{% script %}
echo(username);
{% endscript %}
```

Note that the `echo` pseudo-function encodes its argument before printing it.

# Expression Statements

Any valid expression can be used as a statement in Script Mode, causing it to
be evaluated and the result discarded. That is, the expression will be
evaluated for its side effects only.

# Statements

## `if`

```ginger
{% script %}
if (condition)
  trueStatement;
else
  falseStatement;
{% endscript %}
```

It is generally a good idea to group the `if` branches though, even if they
contain only one statement each:

```ginger
{% script %}
if (condition) {
  trueStatement;
}
else {
  falseStatement;
}
{% endscript %}
```

Unlike C, Ginger's `elif` also works in script mode:

```ginger
{% script %}
if (condition) {
  trueStatement;
}
elif (alternativeCondition) {
  alternativeStatement)
}
else {
  falseStatement;
}
{% endscript %}
```

## `switch`

```ginger
{% script %}
switch (expression) {
  case someValue:
    someStatement;
    someOtherStatment;
    // No break; required nor allowed
  case secondValue:
    secondStatement;
    secondOtherStatment;
  default:
    defaultStatement;
}
{% endscript %}
```

## `set`

```ginger
{% script %}
set varName = expression;
{% endscript %}
```

## `for`

```ginger
{% script %}
for (item in items) {
  statement;
}
else {
  statement;
}
{% endscript %}
```

## `macro`

```ginger
{% script %}
macro macroName(arg1, arg2) {
  statement;
}
{% endscript %}
```

## `include`

```ginger
{% script %}
include("included-file.html");
{% endscript %}
```

## `scope`

```ginger
scope {
  statement;
}
```
