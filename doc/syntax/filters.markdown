% Filters

In Ginger, filters and functions are the same thing, the difference is in the
syntax: `{{ var|filter(arg) }}` is exactly the same thing as `{{ filter(var,
arg) }}`. We provide both for two reasons: one, Jinja2 and Twig *do* make a
difference, and we want to support both use cases; and two, sometimes one is
more readable, sometimes the other.

# List Of Builtin Filters / Functions

Note: all functions are given in function-call syntax; filter syntax is valid
for every one of them as well, just remove the first argument.

## `raw(value)`

Forces Ginger to interpret `value` as pre-encoded HTML source.

## `abs(value)`

The absolute value of a number.

## `any(items)`

Test if any of the `items` is true-ish.

## `all(items)`

Test if all of the `items` is falsy.

## `capitalize(str)`

Upper-cases the first code point in `str`.

## `ceil(number)`

Rounds a `number` to the next larger integer.

## `center(str, width=80, pad=' ')`

Aligns a string `str` to fit in `width` characters. If `str` is shorter than
`width`, it gets truncated; if it is longer, `pad` characters are added on both
sides to make it fit.

## `concat(arg0, arg1, ...)`

Concatenates all arguments into one.

## `contains(list, elem)`

Checks whether `list` contains `elem`.

## `dateformat(date, format, tz=null, locale=null)`

*Alias:* `date`

Converts `datetime` to a proper date/time value, and then formats it according
to the given `format`.

- `date` can be one of:
  - a string containing a preformatted date or date+time, in one of the
    following formats:
    - `%Y-%m-%dT%H:%M:%S%Z`
    - `%Y-%m-%d %H:%M:%S`
    - `%Y-%m-%d %H:%M:%S%z`
    - `%Y-%m-%d %H:%M:%S%Z`
    - `%Y-%m-%d`
  - A list of date parts, in the ordering:
    `[ year, month, day, hour, minute, second, timezone ]`; partial lists are
    allowed, leaving out the timezone or the timezone and the time-of-day part.
- `format` supports UNIX-style date/time format strings, as per [this
specification](http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html#formatTime).
- `tz` can be either a dictionary containing the keys `minutes`,
  `summerOnly`, and `name`, to explicitly define a timezone, or a string
  containing anything the `"%z"` format string understands.
- `locale` is the name of a locale; in order to resolve it, the `getlocale`
  function is called. `getlocale` does not come with Ginger, you have to
  define it yourself and add it to the Ginger execution context. This is
  because we want to keep Ginger agnostic of the chosen host monad, and any
  method of using the OS's locale system would involve `IO` somehow. If
  `getlocale` is not defined, `date` uses a "default locale" (roughly
  equivalent to the "C locale").


## `default(value, def)`

*Alias:* `d`

Return `value` if it is truthy, otherwise return `def`.

## `dictsort(dict, case_sensitive=false, by="key")`

Sort a dictionary-like object by keys (the default) or values (`by="val"`),
keeping key-value relationships intact.

## `difference(arg0, arg1, ...)`

A variadic subtraction function. `difference(a, b, c, d)` is equivalent to `a -
b - c - d`.

## `equals(arg0, arg1, ...)`

A variadic equality comparison function. Returns true iff all arguments are
equal to one another. Uses loose equality.

## `escape()`

*Alias:* `e`

Explicitly HTML-encode `arg`.

## `eval(src, context)`

Evaluate Ginger source code `src` in given `context`, returning the
captured output of the evaluation.

Evaluation runs in a "clean" environment, that is, none of the functions or
variables defined in the current run state are available; `eval` only sees the
standard functions (everything on this page), plus the variables defined
through the `context` parameter.

Because of this "sandboxing", Ginger's `eval` is considerably safer than
comparable functions in, say, JavaScript or PHP.

## `filesizeformat(size_bytes, base2=false)`

Format a file size (given in number of bytes) to a human-readable string. If
`base2` is truthy, use 2-based units (GiB, MiB, kiB), otherwise use 10-based
units (GB, MB, kB). Works up to petabytes (both PiB and PB).

## `filter(list, predicate)`

Filters a `list`, retaining only those elements for which `predicate` returns
`true`.

## `floor(num)`

Returns the largest integer smaller than or equal to `num`.

## `format(fmt, arg0, arg1, ...)`

Alias for `printf`.

## `greater(a, b)`

Same as `a > b`.

## `greaterEquals(a, b)`

Same as `a >= b`.

## `int(a)`

Force a value to an integer. 0 for invalid integers.

## `int_ratio(arg0, arg1, ...)`

Variadic integer division: `int_ratio(a, b, c) == a / b / c`.

## `iterable(x)`

Checks whether `x` is iterable. Anything that implements the list or dict
interface, or both, is considered iterable.

## `length(x)`

Get the length of `x`. For lists and dictionaries, get the number of elements.
For strings, get the number of code points.

## `less(a, b)`

Same as `a < b`.

## `lessEquals(a, b)`

Same as `a <= b`.

## `modulo(a, b, ...)`

Variadic modulo operation. `modulo(a, b, c, d) == a % b % c % d`.

## `nequals(a, b)`

Not-equals: `a != b`.

## `num(x)`

Force `x` to a number (int or float).

## `printf(fmt, arg0, ...)`

Classic `printf`. `fmt` accepts format strings similar to `printf` in libc; for
an exact specification, please refer to [the relevant documentation on
Hackage](http://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Printf.html#v:printf).

## `product(arg0, ...)`

Variadic multiplication.

## `ratio(arg0, ...)`

Variadic division: `ratio(a, b, c, d) == a / b / c / d == a / (b * c * d)`.

## `replace(str, search, replace)`

Find all non-overlapping occurrences of `search` in `str`, and replace them
with `replace`.

## `round(x)`

Round `x` to the nearest integer.

## `show(x)`

*Discouraged*: call Haskell's `show` function on `x`. `Show` for `GVal` is
implemented to look similar to JSON, but it's not actual JSON, so YMMV. You
probably want `str()` instead.

## `slice(slicee, start=0, length=null)`

Take a sublist from `slicee`, starting at (zero-based) position `start`, and
containing up to `length` elements. If `length` is null or unspecified, take
all elements until the end of `slicee`. If `start` is negative, count from the
end of the list instead (pretty much exactly how it works in Python).

## `sort(sortee, by=null, reverse=false)`

Takes a list or dictionary, returns a sorted copy. If `reverse` is trueish,
reverse the list. The `by` argument determined the exact details of the
comparison that is used; by default, the list is sorted alphabetically, but
`by` accepts the following kinds of arguments:

- A string, used as a property looked up in every element.
- A function, called on every element value.
- The magic string `"__key"`, causing the list to be sorted on the key rather
  than the value.
- A list of strings, used as a chain of property to "drill down" into a nested
  dictionary.

## `str(x)`

Force `x` to a plain-text string.

## `sum(arg0, ...)`

A variadic addition.

## `truncate(x)`

Truncate a number: chop off the fractional part.

## `urlencode(str)`

URL-encode `str`.
