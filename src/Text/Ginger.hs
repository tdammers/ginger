{-|
A Haskell implementation of the <http://jinja.pocoo.org/ Jinja2> template
language.

Ginger aims to be as close to the original Jinja language as possible, but
avoiding blatant pythonisms and features that make little sense outside of
an impure dynamic host language context, especially when this would require
sacrificing runtime performance.
-}

module Text.Ginger
(
-- * Template Syntax
-- ** Minimal example template
-- | > <!DOCTYPE html>
--   > <html>
--   >     <head>
--   >         <title>{{ title }}</title>
--   >     </head>
--   >     {# This is a comment. Comments are removed from the output. #}
--   >     <body>
--   >         <menu id="nav-main">
--   >         {% for item in navigation %}
--   >             <li><a href="{{ item.url }}">{{ item.label }}</a></li>
--   >         {% endfor %}
--   >         </menu>
--   >         <div class="layout-content-main">
--   >             <h1>{{ title }}</h1>
--   >             {{ body }}
--   >         </div>
--   >     </body>
--   > </html>

-- There are two kinds of delimiters. `{% ... %}` and `{{ ... }}`. The first
-- one is used to execute statements such as for-loops or assign values, the
-- latter prints the result of an expression to the template.
--
-- /Not implemented yet/: Jinja2 allows the programmer to override the default
-- tags from @{% %}@ and @{{ }}@ to different tokens, e.g. @\<% %\>@ and @\<\<
-- \>\>@.  Ginger does not currently support this.

-- ** Variables
-- | You can mess around with the variables in templates provided they are
-- passed in by the application. Variables may have attributes or elements
-- on them you can access too. What attributes a variable has depends
-- heavily on the application providing that variable.
--
-- You can use a dot (@.@) to access attributes of a variable, but
-- alternatively the so-called “subscript” syntax (@[]@) can be used. The
-- following lines do the same thing:
--
-- > {{ foo.bar }}
-- > {{ foo['bar'] }}
--
-- It’s important to know that the curly braces are /not/ part of the
-- variable, but the print statement. If you access variables inside tags
-- don’t put the braces around them.
--
-- If a variable or attribute does not exist you will get back an undefined
-- value. What you can do with that kind of value depends on the
-- application configuration: the default behavior is that it evaluates to
-- an empty string if printed and that you can iterate over it, but every
-- other operation fails.

-- * Haskell API
-- ** General
-- | On the Haskell side of things, executing a template is a two-step process.
-- First, template source code is parsed into a 'Template' data structure,
-- which is then fed to 'runGinger' or 'runGingerT'.

-- **  Parsing
-- | Because Ginger templates can include other templates, the parser needs a way of
-- resolving template names. Instead of hard-wiring the parser into 'IO' though,
-- Ginger will work on any Monad type, but requires the caller to provide a
-- suitable template resolver function. For 'IO', the resolver would typically
-- load a file from a template directory, but other monads might have access to
-- some sort of cache, or expose template compiled into a program, or simply
-- return 'Nothing' unconditionally to disable any and all imports. A suitable
-- example implementation for 'IO' would look like this:

-- | > loadFile fn = openFile fn ReadMode >>= hGetContents
-- > 
-- > loadFileMay fn =
-- >     tryIOError (loadFile fn) >>= \e ->
-- >          case e of
-- >             Right contents ->
-- >                 return (Just contents)
-- >             Left err -> do
-- >                 print err -- remove this line if you want to fail silently
-- >                 return Nothing

-- | (Taken from @cli/GingerCLI.hs@). This interprets the template name as a
-- filename relative to the CWD, and returns the file contents on success or
-- 'Nothing' if there is any error.

-- | If you don't need a monadic context for resolving includes (e.g. because you
-- have pre-loaded all template sources), you can use the pure 'parseGinger'
-- flavor, which does not rely on a host monad.
  module Text.Ginger.Parse
-- ** Running
-- | The core function for running a template is 'runGinger' (or its monadic
-- flavor 'runGingerT'); in order to pass an initial context to the template
-- engine, pass a suitable 'GingerContext', which you can create using the
-- 'makeContext' / 'makeContextM' functions.

-- | An example call (for running a template in 'IO') would look something like
-- this:
--
-- > runGingerT (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) tpl
, module Text.Ginger.Run
-- ** Other concerns
-- | Ginger's unitype value
, module Text.Ginger.GVal
-- | The data structures used to represent templates, statements and
-- expressions internally.
, module Text.Ginger.AST
)
where
import Text.Ginger.Parse
import Text.Ginger.AST
import Text.Ginger.Run
import Text.Ginger.GVal


