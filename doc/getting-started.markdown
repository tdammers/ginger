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

## Running

The core function for running a template is 'runGinger' (or its monadic
flavor 'runGingerT'); in order to pass an initial context to the template
engine, pass a suitable 'GingerContext', which you can create using the
'makeContext' / 'makeContextM' functions.

An example call (for running a template in 'IO') would look something like
this:

    runGingerT (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) tpl

## Further Reading

For full documentation on Ginger's Haskell API, check out the Haddock
documentation that ships with it, which is also available on Hackage:
https://hackage.haskell.org/package/ginger
