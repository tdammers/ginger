# Ginger


[![Build Status](https://travis-ci.org/tdammers/ginger.svg?branch=master)](https://travis-ci.org/tdammers/ginger)
[![Hackage](https://img.shields.io/hackage/v/ginger.svg)](https://hackage.haskell.org/package/ginger)

![](http://ginger.tobiasdammers.nl/static/img/ginger-leaf.svg)

A Haskell implementation of the [Jinja2](http://jinja.pocoo.org/) template
language.

> A note of warning: the git repository at https://bitbucket.org/tdammers/ginger
> has been deleted and restored with a rewritten commit tree on 2016-04-06 in
> order to clean up the messy history. This means that if you have a checkout
> from before that date, merging the bitbucket repo will most likely break
> things; please do a fresh clone to fix this. Sorry for the inconvenience.

## Introduction

Ginger provides most of the original Jinja2 template language, as much as that
makes sense in a Haskell host application.

We do, however, some of the most blatant Pythonisms, especially when we felt
the features in question were more of an accidental result of binding template
constructs to Python constructs.

On top of that, we deviate on a few points, and add some features that we think
are very useful and help make the language better and more consistent.

## Installation

Ginger is available from [Hackage](https://hackage.haskell.org/package/ginger),
and it is in Stackage, so you can use it in plain Cabal projects as well as
Stack ones.

### Installing with Cabal:

    cabal install ginger

### Installing with Stack:

    stack install ginger

### Using as part of another project:

Add the following to your `.cabal`'s `build-depends`:

    ginger >=0.3.9.1 && <0.4

## Template Syntax

Full template syntax documentation is available from the `/docs` subdirectory
in the project repository itself, or from [the User Guide section on the
Ginger website](https://ginger.tobiasdammers.nl/guide).

### Minimal example template

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

There are two kinds of delimiters. `{% ... %}` and `{{ ... }}`. The first
one is used to execute statements such as for-loops or assign values, the
latter prints the result of an expression to the template.

*Not implemented yet*: Jinja2 allows the programmer to override the default
tags from `{% %}` and `{{ }}` to different tokens, e.g. `<% %>` and `<< >>`.
Ginger does not currently support this.

## Haskell API

The Haskell API is documented fully through Haddock documentation, available
from [Hackage](https://hackage.haskell.org/package/ginger). We'll just provide
a short overview here.

### General

On the Haskell side of things, executing a template is a two-step process.
First, template source code is parsed into a 'Template' data structure,
which is then fed to 'runGinger' or 'runGingerT'.

###  Parsing

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

### Running

The core function for running a template is 'runGinger' (or its monadic
flavor 'runGingerT'); in order to pass an initial context to the template
engine, pass a suitable 'GingerContext', which you can create using the
'makeContext' / 'makeContextM' functions.

An example call (for running a template in 'IO') would look something like
this:

    runGingerT (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) tpl
