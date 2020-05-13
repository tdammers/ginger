## 0.9.0.0

- Added `split()` builtin
- Added support for byte arrays, represented as `ByteString` on the Haskell
  side

## 0.8.4.0

- Added builtin `apply`, making it possible to pass argument lists as arrays

## 0.8.3.0

- Added builtin `regex` module for POSIX regular expressions support

## 0.8.2.0

- Expose some internals of the `Run` type and the default implementations of
  built-in functions / filters

## 0.8.1.0

- Added built-ins: `partial`, `zip`, `zipwith`, `compose`

## 0.8.0.0

- Now compiles on GHC 8.4
- CLI: added a `system()` template function (for spawning subprocesses)
- CLI accepts YAML
- Added `is` operator (also makes Ginger conform to Jinja2 test syntax)
- Various bugfixes
- New builtins: `escaped`, `in`, Python-style boolean operators, `divisibleBy`,
  `even`, `odd`, and more
- Boolean literals now also accepted in caps
- Improved documentation
- `GVal` instances for `Integer`
- Overridable delimiters

## 0.7.4.0

- Make concat() / ~ more generic (now also concatenates lists and dictionaries)
- CLI omits printing of `null` results. Useful when using as a filter.
- Fixed excessive newlines in CLI output

## 0.7.3.0

- Expose parser error position details

## 0.7.2.0

- '|json' filter

## 0.7.1.0

- `StripBlocks` and `LTrimBlocks` options
- `+` tag modifier to override whitespace stripping

## 0.7.0.0

- `keepTrailingNewlines` option
- Ability to pass parser options into parseGinger
- Runtime warnings

## 0.6.0.2

- Documentation fixes

## 0.6.0.1

- Haddock documentation fix

## 0.6.0.0

- Exceptions / exception handling.

## 0.5.3.0

- Marshalling and hoisting: it is now possible to fully marshal `GVal`s between
  arbitrary carrier monads, as long as suitable conversion functions are
  provided.

## 0.5.2.0

- Added map(), upper(), lower() functions

## 0.5.1.3

- Documentation fixes

## 0.5.1.2

- Release-related fixups

## 0.5.1.1

- Bugfixes wrt indentation mode

## 0.5.1.0

- Expose parser error pretty-printer from the library

## 0.5.0.0

- Indentation mode: `{% indent %}` introduces an indentation context

## 0.4.0.0

- Statements can now return values
- Added `do` expressions (lift statements into expressions)

## 0.3.11.1

- Fixed a parser bug related to whitespace in script mode

## 0.3.11.0

- Fixed the way local scopes work in script mode
- Documented script mode

## 0.3.10.0

- Script mode: alternative syntax that makes it easier to use
  Ginger as a scripting language, used inside {% script %} blocks.

## 0.3.9.1

- Various dependency issues fixed

## 0.3.8.0

- Added a `{% switch %}` statement
