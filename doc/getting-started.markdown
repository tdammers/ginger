% Getting Started

# Installation

The easiest way to use Ginger in Haskell projects is through cabal:

```sh
cabal install ginger
```

Stack is also supported; ginger is not in stackage, but adding it as an extra
dependency will work fine. Alternatively, you can also hook the git repository
into `stack.yml`.

# A Basic Template

```ginger
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
```

There are two kinds of delimiters. `{% ... %}` and `{{ ... }}`. The first
one is used to execute statements such as for-loops or assign values, the
latter prints the result of an expression to the template. To users of Jinja,
Twig, or Django, this should be instantly familiar.

# Haskell Code

On the Haskell side of things, executing a template is a two-step process.
First, template source code is parsed into a 'Template' data structure,
which is then fed to 'runGinger' or 'runGingerT'.

##  Parsing

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

## Running - The Easy Interface

Ginger's "easy" interface is rather simple, offering just two flavors of a
template rendering function, monadic (`easyRenderM`) and effect-less
(`easyRender`). Using them is as simple as it gets, and a simple example should
make things clear:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Ginger
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad.Identity (runIdentity)

-- | A template context. We're using a HashMap of Text to Text, but any type
-- that has a suitable 'ToGVal' instance would do.
context :: HashMap Text Text
context = HashMap.fromList
    [ ("name", "Alice")
    , ("location", "Wonderland")
    ]

-- | We don't need to support includes, so we'll create an include resolver
-- that always fails. If you need to use includes, you'll want to use an actual
-- resolver here (see the next section for an example implementation), and
-- use a suitable monad for the parsing step (e.g. 'IO').
nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

-- | This is our template. Because 'parseGinger' wants a monad (as loading
-- includes would normally go through some sort of monadic API like 'IO'), we
-- use 'Identity' here.
template :: Template 
template = either error id . runIdentity $
  parseGinger nullResolver "Hello, {{ name }}, welcome in {{ location }}!"

main = do
  let output = easyRender context template
  Text.putStrLn output
```

Note that, because Ginger is a dynamically typed language (or, actually, an
untyped language), passing values to a template execution context requires
conversion to Ginger values, represented on the Haskell side as the `GVal`
type. The `ToGVal` typeclass exposes a number of convenience functions that can
be used for this purpose, and there are instances for many of Haskell's
standard data types.

## Running - The Raw Interface

The raw interface gives you more control of the context passing, output
writing, encoding, etc.

The core function for running a template is 'runGinger' (or its monadic
flavor 'runGingerT'); in order to pass an initial context to the template
engine, pass a suitable 'GingerContext', which you can create using the
'makeContext' / 'makeContextM' functions.

An example call (for running a template in 'IO') would look something like
this:

```haskell
runGingerT (makeContextHtmlM scopeLookup (putStr . Text.unpack . htmlSource)) tpl
```

To dissect it:

- `scopeLookup` should be a function that takes a `Text` key and returns a
  suitable `GVal` into the `Run` monad. This is slightly more generic than
  passing in the entire context at once, as it allows you to load values on
  demand, rather than having all the values ready upfront.
- The `putStr . Text.unpack . htmlSource` part is how you want Ginger to
  implement output writing, that is, this function is called once for every bit
  of output the template generates. In this example, we're getting the raw HTML
  source out, convert it to a `String`, and print it on stdout - but in a web
  application, we might instead send it to the HTTP response, and in a static
  site generator, we'd write it to a file.
- `tpl` is the template we want to run.

### A Worked Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.HashMap.Strict (fromList, HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import System.IO.Error (tryIOError)
import Text.Ginger
       (makeContextHtml, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Text.Ginger.Html (htmlSource)


-- A simple hashmap that we'll use as our template context
sampleContext :: HashMap Text Text
sampleContext = fromList [("name", "Alice")]


-- Given a Template and a HashMap of context, render the template to Text
render :: Template -> HashMap VarName Text -> Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template


-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup
  :: (Hashable k, Eq k, ToGVal m b)
  => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context


loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing

  where
    loadFile :: FilePath -> IO String
    loadFile fn' = openFile fn' ReadMode >>= hGetContents


-- Assuming there's an html file called "base.html" in the current directory and
-- that html file's contents are `Hi, {{ name }}`, attempt to parse "base.html"
-- and print the rendered template
-- >>> run
-- "Hi, Alice"
main :: IO ()
main = do
  template <- parseGingerFile loadFileMay "base.html"
  case template of
    Left err -> print err >> exitFailure
    Right template' -> print $ render template' sampleContext

```

## Further Reading

For full documentation on Ginger's Haskell API, check out the Haddock
documentation that ships with it, which is also available on Hackage:
https://hackage.haskell.org/package/ginger
