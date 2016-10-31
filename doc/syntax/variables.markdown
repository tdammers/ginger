% Variables

Variable interpolation, that is, injecting context variables into the template
output, is done with the `{{ }}` syntax:

```ginger
<div>{{ some_value }}</div>
```

Variable interpolation is not restricted to simple variables; any valid
[Expression](/syntax/expressions) can be used in an interpolation.

## A Note On Values

Ginger is a dynamically typed language; conceptually, any Ginger
value has at least a text representation and an HTML representation, but many
values also support a list-like API, dictionary-style key-based access, or
call-as-function semantics. 
