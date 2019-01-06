% Getting Started

# Installation

The easiest way to use Ginger in Haskell projects is through cabal:

```sh
cabal install ginger
```

Stack is also supported; ginger is in Stackage, so it should install just fine.
You may find that the version in Stackage lags a bit behind, due to the way
Stackage's LTS releases work. You may want to add Ginger as an `extra-deps`
entry, or even hook the git repository into `stack.yml`. Please refer to
`stack` documentation for details.

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

Paste this in a file named `test.html`. Then create another file called
`test.json`, containing this:

```json
{
    "navigation": [
        {
            "label": "Home",
            "url": "/"
        },
        {
            "label": "Example Site",
            "url": "https://www.example.com/hello"
        },
        {
            "label": "Ginger Website",
            "url": "https://ginger.tobiasdammers.nl/"
        }
    ],
    "title": "Hello, World!"
}
```

We can pass this to the `ginger` CLI executable, like so:

```shell
$ ginger test.html test.json
<!DOCTYPE html>
<html>
    <head>
        <title>Hello, World!</title>
    </head>
    
    <body>
        <menu id="nav-main">
                    <li><a href="/">Home</a></li>
                    <li><a href="https://www.example.com/hello">Example Site</a></li>
                    <li><a href="https://ginger.tobiasdammers.nl/">Ginger Website</a></li>
                </menu>
        <div class="layout-content-main">
            <h1>Hello, World!</h1>
            
        </div>
    </body>
</html>
```

Now go play with the template (`test.html`) and the input data (`test.json`),
and see if you can make it do interesting things.

## Ginger Syntax: The Basics.

There are two kinds of delimiters. `{% ... %}` and `{{ ... }}`. The first
one is used to execute statements such as for-loops or assign values, the
latter prints the result of an expression to the template. To users of Jinja,
Twig, or Django, this should be instantly familiar. If you're new to these,
however, here's a quick rundown:

- `{{ expression }}` takes the value of an `expression` and injects it into
  the output at the same position, with automatic HTML-encoding. Ginger has
  a fully-featured expression language, so things like addition (`a + b`),
  property access (`a["foo"]` or `a.foo`), etc., will work. You can see this in
  action in our example template.
- `{% for loopVar in list  %} ... {% endfor %}` is a loop construct, roughly
  equivalent to `forM` in Haskell or `foreach` in a typical imperative
  language. Our example uses it to iterate over the `navigation` variable.
- `{% if expression %} ... {% else %} ... {% endif %}` is a conditional, and
  it works exactly like you'd expect - if `expression` evaluates to something
  truthy, the first branch is rendered, otherwise the second branch is
  rendered.

# Haskell Code

So far, we've been using the command-line `ginger` tool, but of course the real
beef is using Ginger in a host application. A good way to get an idea of how
that works is to look at the code in `cli/GingerCLI.hs`, the module that
defines the `ginger` command-line tool.

##  Parsing

The first thing we'll need to do is to parse some input into a Ginger
`Template`. Templates are polymorphic over the source code position; but this
is an implementation you can safely ignore for now. Let's look at
`parseGinger`:

parseGinger :: forall m. Monad m => IncludeResolver m -> Maybe SourceName -> Source -> m (Either ParserError (Template SourcePos))
    parseGinger :: forall m. Monad m
                => IncludeResolver m
                -> Maybe SourceName
                -> Source
                -> m (Either ParserError (Template SourcePos))

First of all, this function is parametric over `m`, a Monad instance in which
the whole thing will run. We would need this to load includes, but since we're
not going to do that for now, we will pick `Identity`.

`IncludeResolver m` is a synonym for `SourceName -> m (Maybe Source)`, but
since we won't be handling includes yet, we can use `(const $ return Nothing)`:
we simply fail to load any and all includes.

The `Maybe SourceName` parameter is used to display source file names in error
messages; we'll pass `Nothing`, because we don't have one.

The `Source` argument is the actual source code, and it's currently a synonym
for `String`.

Armed with all this, we can write the parsing part:

```haskell
module Main where

import Text.Ginger
import Control.Monad.Identity
import System.IO

main = do
  src <- getContents
  let result = runIdentity $
        parseGinger
          (const $ return Nothing) -- include resolver
          Nothing                  -- source name
          src
  case result of
    Left err -> hPutStrLn stderr $ show err
    Right result -> print result
```

## Running - The Easy Interface

Ginger's "easy" interface is rather simple, offering just two flavors of a
template rendering function, monadic (`easyRenderM`) and effect-less
(`easyRender`). Using them is as simple as it gets, and a simple example should
make things clear:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Identity (Identity, runIdentity)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Ginger

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
template :: Template SourcePos
template = either (error . show) id . runIdentity $
  parseGinger nullResolver Nothing "Hello, {{ name }}, welcome in {{ location }}!"

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
       (SourcePos, makeContextHtml, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Text.Ginger.Html (htmlSource)


-- A simple hashmap that we'll use as our template context
sampleContext :: HashMap Text Text
sampleContext = fromList [("name", "Alice")]


-- Given a Template and a HashMap of context, render the template to Text
render :: Template SourcePos -> HashMap VarName Text -> Text
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
