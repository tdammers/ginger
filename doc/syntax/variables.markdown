% Variables

Variable interpolation, that is, injecting context variables into the template
output, is done with the `{{ }}` syntax:

```ginger
<div>{{ some_value }}</div>
```

You can use a dot (`.`) to access attributes of a variable, but
alternatively the so-called “subscript” syntax (`[]`) can be used. The
following lines do the same thing:

    {{ foo.bar }}
    {{ foo['bar'] }}

It’s important to know that the curly braces are *not* part of the
variable, but the print statement. If you access variables inside tags
don’t put the braces around them.

If a variable or attribute does not exist you will get back an undefined
value, which will render as nothing, evaluate to "false" in comparisons, and
amount to 0 (zero) in numeric calculations.

In fact, there is a lot more you can do with variables; any valid
[Expression](/syntax/expressions) can be used in an interpolation, and Ginger's
expression language is quite powerful.

## A Note On Values

Ginger is a dynamically typed language; conceptually, any Ginger
value has at least a text representation and an HTML representation, but many
values also support a list-like API, dictionary-style key-based access, or
call-as-function semantics.

Ginger will pick the most suitable representation depending on context; this
means that when you interpolate things into HTML templates, Ginger will either
pick the HTML representation of the value, or, when that doesn't exist, it will
pick the plain-text representation and HTML-encode it.

You can override this behavior in a few ways:

- The `|str` filter (cf. [Filters](/syntax/filters)) forces Ginger to pick the
  text representation, even if a HTML representation exists. Typically, this
  will amount to stripping all markup from the HTML and retaining just the
  text content, however, values can implement this differently in order to
  produce a better textual representation.
- The `|raw` filter **DISABLES HTML ENCODING ENTIRELY**. This is normally
  dangerous, but there are sometimes exceptional cases where you have HTML
  source in a string, and you can be sure that it doesn't contain any malicious
  / user-supplied data; the `|raw` filter can then be used to tell Ginger that
  the value is already HTML-encoded and can be considered "safe".
