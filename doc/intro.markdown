% Introduction

# What Is Ginger

Ginger is a Haskell implementation of the [Jinja2](http://jinja.pocoo.org/)
template language.

Ginger aims to be as close to the original Jinja language as possible, but
avoiding blatant pythonisms and features that make little sense outside of
an impure dynamic host language context, especially when this would require
sacrificing runtime performance.

# Minimal example template

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
