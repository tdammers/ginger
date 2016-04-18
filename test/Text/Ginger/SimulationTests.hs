{-#LANGUAGE OverloadedStrings #-}
module Text.Ginger.SimulationTests
where

import Text.Ginger
import Text.Ginger.Html
import Test.Tasty
import Test.Tasty.HUnit
import Data.Default (def)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Monoid

simulationTests :: TestTree
simulationTests = testGroup "Simulation"
    [ testCase "Smoke Test" $ mkTest [] [] "Hello" "Hello"
    , testGroup "Comments"
        -- There is a comment between the two dashes that should not appear in
        -- the output.
        [ testCase "Comment does not appear in output" $ mkTest
            [] [] "- {# Comments #} -" "-  -"
        -- A comment tag ending with -#} instead of #} should eat subsequent
        -- whitespace, and a comment tag starting with {#- instead of {# should
        -- eat preceding whitespace.  There is such a comment between the
        -- following dashes, so they should appear on the same line, with no
        -- space between them:
        , testCase "Dashed comments eat whitespace" $ mkTest
            [] [] "- {#- Comment -#} -" "--"
        ]
    , testGroup "Literals"
        [ testCase "String: \"foobar\"" $ mkTest
            [] [] "{{ \"foobar\" }}" "foobar"
        , testGroup "Numbers"
            [ testCase "123" $ mkTest
                [] [] "{{ 123 }}" "123"
            , testCase "3.1415" $ mkTest
                [] [] "{{ 3.1415 }}" "3.1415"
            ]
        , testGroup "Booleans"
            [ testCase "true" $ mkTest
                [] [] "{{ true }}" "1"
            , testCase "false" $ mkTest
                [] [] "{{ false }}" ""
            ]
        , testCase "Null" $ mkTest
            [] [] "{{ null }}" ""
        ]
    , testGroup "Simple list/object constructs"
        [ testCase "Lists" $ mkTest
            [] [] "{{ [\"foo\",\"bar\",\"baz\" ] }}" "foobarbaz"
        , testCase "Nested lists" $ mkTest
            [] [] "{{ [ \"foo\", \"bar\", [ 1, 2, 3 ], \"baz\" ] }}" "foobar123baz"
        , testCase "Objects" $ mkTest
            [] [] "{{ { \"foo\":\"bar\" } }}" "bar"
        , testCase "Nested object/list constructs" $ mkTest
            [] [] "{{ { \"foo\":[\"foo\", {\"asdf\" : \"bar\"}, [\"baz\" ]] } }}" "foobarbaz"
        ]
    , testGroup "Accessing object/list members"
        [ testCase "by integer index" $ mkTest
            [] [] "{{ [ \"foo\", \"bar\" ][1] }}" "bar"
        , testCase "by string key" $ mkTest
            [] [] "{{ { \"foo\": \"bar\", \"baz\": \"quux\" }['foo'] }}" "bar"
        , testCase "by property name" $ mkTest
            [] [] "{{ { \"foo\": \"bar\", \"baz\": \"quux\" }.foo }}" "bar"
        , testCase "multi-level mixed" $ mkTest
            [] [] "{{ { \"foo\": { \"oink\": \"nope\", \"baz\": { \"boop\": [], \"quux\": \"bar\" }}}.foo.baz[\"quux\"] }}" "bar"
        ]
    , testGroup "Function calls"
        -- In order to make sure the correct function is called, we write one
        -- that stores its argument in an IORef that it closes over, thus
        -- making it impossible for other functions to modify it; then we
        -- assert that the IORef contains what we expect.
        [ testCase "print(\"Hello\")" $ do
            buf <- newIORef ""
            let printF :: Function (Run IO Html)
                printF = \xs -> do
                    liftIO . writeIORef buf . mconcat . map (asText . snd) $ xs
                    return def
            mkTest [("print", fromFunction printF)] [] "{{ print(\"Hello\") }}" ""
            actual <- readIORef buf
            let expected = "Hello"
            assertEqual "" actual expected
        , testCase "\"Hello\"|print" $ do
            buf <- newIORef ""
            let printF :: Function (Run IO Html)
                printF = \xs -> do
                    liftIO . writeIORef buf . mconcat . map (asText . snd) $ xs
                    return def
            mkTest [("print", fromFunction printF)] [] "{{ \"Hello\"|print }}" ""
            actual <- readIORef buf
            let expected = "Hello"
            assertEqual "" actual expected
        ]
    , testGroup "Addition"
        [ testCase "1 + 1 = 2" $ do
            mkTest [] [] "{{ sum(1, 1) }}" "2"
        , testCase "1 + 1 = 2" $ do
            mkTest [] [] "{{ 1 + 1 }}" "2"
        ]
    , testGroup "Subtraction"
        [ testCase "1 - 1 = 0" $ do
            mkTest [] [] "{{ 1 - 1 }}" "0"
        ]
    , testGroup "Concatenation"
        [ testCase "1 ~ \"foo\" = 1foo" $ do
            mkTest [] [] "{{ 1 ~ \"foo\" }}" "1foo"
        ]
    , testGroup "Multiplication"
        [ testCase "5 * 5 = 25" $ do
            mkTest [] [] "{{ 5 * 5 }}" "25"
        ]
    , testGroup "Division"
        [ testCase "24 / 6 = 4" $ do
            mkTest [] [] "{{ 24 / 6 }}" "4"
        , testCase "3 / 2 = 1.5" $ do
            mkTest [] [] "{{ 3 / 2 }}" "1.5"
        ]
    , testGroup "Integer Division"
        [ testCase "24 // 6 = 4" $ do
            mkTest [] [] "{{ 24 // 6 }}" "4"
        , testCase "3 // 2 = 1" $ do
            mkTest [] [] "{{ 3 // 2 }}" "1"
        ]
    , testGroup "Modulo"
        [ testCase "7 % 3 = 2" $ do
            mkTest [] [] "{{ 7 % 3 }}" "1"
        ]
    , testGroup "Iteration"
        [ testCase "for x in [ \"foo\", \"bar\", \"baz\" ]: <x>" $ do
            mkTest [] [] "{% for x in [ \"foo\", \"bar\", \"baz\" ] %}<{{x}}>{% endfor %}" "<foo><bar><baz>"
        , testCase "for x in []: <x> else <no>" $ do
            mkTest [] [] "{% for x in [] %}<{{x}}>{% else %}<no>{% endfor %}" "<no>"
        , testCase "for x in [a]: <x> else <no>" $ do
            mkTest [] [] "{% for x in [\"a\"] %}<{{x}}>{% else %}<no>{% endfor %}" "<a>"
        ]
    , testGroup "The `loop` auto-variable"
        [ testCase "loop.cycle" $ do
            mkTest [] []
                ( "{% for x in [\"foo\", \"bar\", \"baz\"] recursive -%}" ++
                  "({{ loop.cycle(\"red\", \"green\") }}/{{ x }})" ++
                  "{%- endfor %}"
                )
                "(red/foo)(green/bar)(red/baz)" 
        , testCase "recursive loops" $ do
            mkTest [] []
                ( "{% for k, x in {\"a\":{\"b\":null,\"c\":{\"d\":null}}} -%}" ++
                  "{{ k }}@{{ loop.depth }}({{ loop(x) }})" ++
                  "{%- endfor %}" :: String
                )
                "a@1(b@2()c@2(d@3()))"
        ]
    , testGroup "Conditionals"
        [ testCase "if true then \"yes\" else \"no\"" $ do
            mkTest [] [] "{% if true %}yes{% else %}no{% endif %}" "yes"
        , testCase "if false then \"yes\" else if false then \"maybe\" else \"no\"" $ do
            mkTest [] [] "{% if false %}yes{% elif false %}maybe{% else %}no{% endif %}" "no"
        , testCase "if false then \"yes\" else if true then \"maybe\" else \"no\"" $ do
            mkTest [] [] "{% if false %}yes{% elif true %}maybe{% else %}no{% endif %}" "maybe"
        ]
    , testGroup "Comparisons"
        [ testCase "if 1 == 1 then \"yes\" else \"no\"" $ do
            mkTest [] [] "{% if (1 == 1) %}yes{% else %}no{% endif %}" "yes"
        , testCase "if 1 > 0 then \"yes\" else \"no\"" $ do
            mkTest [] [] "{% if (1 > 0) %}yes{% else %}no{% endif %}" "yes"
        , testCase "if 1 > null then \"yes\" else \"no\"" $ do
            mkTest [] [] "{% if (1 > null) %}yes{% else %}no{% endif %}" "no"
        , testCase "if 1 < 2 then \"yes\" else \"no\"" $ do
            mkTest [] [] "{% if (1 < 2) %}yes{% else %}no{% endif %}" "yes"
        , testCase "if null < 1 then \"yes\" else \"no\"" $ do
            mkTest [] [] "{% if (null < 1) %}yes{% else %}no{% endif %}" "no"
        ]
    , testGroup "Boolean AND"
        [ testCase "AND (both)" $ do
            mkTest [] [] "{% if 1 && 2 %}yes{% else %}no{% endif %}" "yes"
        , testCase "AND (only one)" $ do
            mkTest [] [] "{% if 1 && 0 %}yes{% else %}no{% endif %}" "no"
        , testCase "AND (neither)" $ do
            mkTest [] [] "{% if 0 && 0 %}yes{% else %}no{% endif %}" "no"
        ]
    , testGroup "Boolean AND"
        [ testCase "OR (both)" $ do
            mkTest [] [] "{% if 1 || 2 %}yes{% else %}no{% endif %}" "yes"
        , testCase "OR (only one)" $ do
            mkTest [] [] "{% if 1 || 0 %}yes{% else %}no{% endif %}" "yes"
        , testCase "OR (either)" $ do
            mkTest [] [] "{% if 0 || 0 %}yes{% else %}no{% endif %}" "no"
        ]
    , testGroup "Built-in filters/functions"
        [ testCase "\"abs\"" $ do
            mkTest [] [] "{{ -2|abs }}" "2"
        , testGroup "\"any\""
            [ testCase "\"any\" (both)" $ do
                mkTest [] [] "{% if any(1, 1, true) %}yes{% else %}no{% endif %}" "yes"
            , testCase "\"any\" (just one)" $ do
                mkTest [] []
                    "{% if any(0, 1, false) %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "\"any\" (neither)" $ do
                mkTest [] []
                    "{% if any(0, 0, false) %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"all\""
            [ testCase "\"all\" (both)" $ do
                mkTest [] []
                    "{% if all(1, 1, true) %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "\"all\" (just one)" $ do
                mkTest [] []
                    "{% if all(0, 1, false) %}yes{% else %}no{% endif %}"
                    "no"
            , testCase "\"all\" (neither)" $ do
                mkTest [] []
                    "{% if all(0, 0, false) %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"ceil\""
            [ testCase "14.1" $ do
                mkTest [] []
                    "{{ 14.1|ceil }}"
                    "15"
            , testCase "-14.1" $ do
                mkTest [] []
                    "{{ -14.1|ceil }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTest [] []
                    "{{ -14.8|ceil }}"
                    "-14"
            ]
        , testCase "\"capitalize\"" $ do
            mkTest [] []
                "{{ \"this is the end of the world\"|capitalize }}"
                "This is the end of the world"
        , testGroup "\"center\"" 
            [ testCase "extra space" $ do
                mkTest [] []
                    "{{ \"asdf\"|center(12) }}"
                    "    asdf    "
            , testCase "no extra space" $ do
                mkTest [] []
                    "{{ \"foobar\"|center(2) }}"
                    "foobar"
            ]
        , testCase "\"concat\"" $ do
            mkTest [] []
                "{{ [\"hello\", \"world\"]|concat }}"
                "helloworld"
        , testGroup "\"contains\""
            [ testCase "single match" $ do
                mkTest [] []
                    "{% if ['hello', 'world']|contains('hello') %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "multi match" $ do
                mkTest [] []
                    "{% if ['hello', 'world']|contains('hello', 'world') %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "non-match" $ do
                mkTest [] []
                    "{% if ['hello', 'world']|contains('hello', 'you', 'world') %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"default\"" 
            [ testCase "trigger default" $ do
                mkTest [] []
                    "{{ 0|default(\"hi\") }}"
                    "hi"
            , testCase "use truthy value" $ do
                mkTest [] []
                    "{{ \"hi\"|default(\"nope\") }}"
                    "hi"
            ]
        , testCase "\"difference\"" $ do
            mkTest [] []
                "{{ difference(5,2) }}"
                "3"
        , testGroup "\"escape\""
            [ testCase "single item" $ do
                mkTest [] []
                    "{{ escape('<')|raw }}"
                    "&lt;"
            , testCase "multiple items" $ do
                mkTest [] []
                    "{{ escape('<', '>')|raw }}"
                    "&lt;&gt;"
            ]
        , testGroup "\"equals\""
            [ testCase "all equal" $ do
                mkTest [] []
                    "{% if equals(1, 1, 1) %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "some equal" $ do
                mkTest [] []
                    "{% if equals(1, 1, 2) %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"filesizeformat\""
            [ testCase "bytes" $ do
                mkTest [] []
                    "{{ 100|filesizeformat }}"
                    "100 B"
            , testCase "kilobytes" $ do
                mkTest [] []
                    "{{ 12000|filesizeformat }}"
                    "12 kB"
            , testCase "megabytes" $ do
                mkTest [] []
                    "{{ 1500000|filesizeformat }}"
                    "1.5 MB"
            , testCase "bytes (2-based)" $ do
                mkTest [] []
                    "{{ 100|filesizeformat(true) }}"
                    "100 B"
            , testCase "kibibytes" $ do
                mkTest [] []
                    "{{ 12000|filesizeformat(true) }}"
                    "11.7 kiB"
            , testCase "mebibytes" $ do
                mkTest [] []
                    "{{ 1500000|filesizeformat(true) }}"
                    "1.4 MiB"
            ]
        , testGroup "\"filter\""
            [ testCase "simple case" $ do
                mkTest [] []
                    "{{ [1, 0, 3]|filter(int) }}"
                    "13"
            , testCase "with extra argument" $ do
                mkTest [] []
                    "{{ [1, 2, 3]|filter(greater, 2) }}"
                    "3"
            ]
        , testGroup "\"not-equals\""
            [ testCase "all equal" $ do
                mkTest [] []
                    "{% if nequals(1, 1, 1) %}yes{% else %}no{% endif %}"
                    "no"
            , testCase "not all equal" $ do
                mkTest [] []
                    "{% if nequals(1, 1, 2) %}yes{% else %}no{% endif %}"
                    "yes"
            ]
        , testGroup "\"floor\""
            [ testCase "14.1" $ do
                mkTest [] []
                    "{{ 14.1|floor }}"
                    "14"
            , testCase "14.8" $ do
                mkTest [] []
                    "{{ 14.8|floor }}"
                    "14"
            , testCase "-14.1" $ do
                mkTest [] []
                    "{{ -14.1|floor }}"
                    "-15"
            , testCase "-14.8" $ do
                mkTest [] []
                    "{{ -14.8|floor }}"
                    "-15"
            ]
        , testGroup "\"int\""
            [ testCase "14.1" $ do
                mkTest [] []
                    "{{ 14.1|int }}"
                    "14"
            , testCase "14.8" $ do
                mkTest [] []
                    "{{ 14.8|int }}"
                    "14"
            , testCase "-14.1" $ do
                mkTest [] []
                    "{{ -14.1|int }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTest [] []
                    "{{ -14.8|int }}"
                    "-14"
            ]
        -- \"int_ratio\"
        -- TODO
        -- \"iterable\"
        -- TODO
        , testCase "\"length\"" $ do
            mkTest [] []
                "{{ [1,2,3]|length }}"
                "3"
        -- \"modulo\"
        -- \"num\"
        -- TODO
        -- \"printf\"
        , testGroup "\"printf\""
            [ testCase "%s, passed as int" $ do
                mkTest [] []
                    "{{ printf(\"%i\", 1) }}"
                    "1"
            , testCase "%s, passed as string" $ do
                mkTest [] []
                    "{{ printf(\"%i\", \"1\") }}"
                    "1"
            , testCase "%i, passed as float" $ do
                mkTest [] []
                    "{{ printf(\"%i\", 1.3) }}"
                    "1"
            , testCase "%f, passed as int" $ do
                mkTest [] []
                    "{{ printf(\"%f\", 1) }}"
                    "1.0"
            , testCase "%.3f, passed as int" $ do
                mkTest [] []
                    "{{ printf(\"%.3f\", 1) }}"
                    "1.000"
            , testCase "%s, string" $ do
                mkTest [] []
                    "{{ printf(\"%s\", \"Hello\") }}"
                    "Hello"
            ]
        , testCase "\"product\"" $ do
            mkTest [] []
                "{{ product(1,2,3) }}"
                "6"
        , testCase "\"ratio\"" $ do
            mkTest [] []
                "{{ ratio(6, 1.5, 2) }}"
                "2"
        , testGroup "\"round\""
            [ testCase "14.1" $ do
                mkTest [] []
                    "{{ 14.1|round }}"
                    "14"
            , testCase "14.8" $ do
                mkTest [] []
                    "{{ 14.8|round }}"
                    "15"
            , testCase "-14.1" $ do
                mkTest [] []
                    "{{ -14.1|round }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTest [] []
                    "{{ -14.8|round }}"
                    "-15"
            ]
        -- \"show\"
        -- TODO
        , testCase "\"str\"" $ do
            mkTest [] []
                "{{ str(123) }}"
                "123"
        , testCase "\"sum\"" $ do
            mkTest [] []
                "{{sum(1, 2, 3)}}"
                "6"
        , testGroup "\"truncate\""
            [ testCase "14.1" $ do
                mkTest [] []
                    "{{ 14.1|truncate }}"
                    "14"
            , testCase "14.8" $ do
                mkTest [] []
                    "{{ 14.8|truncate }}"
                    "14"
            , testCase "-14.1" $ do
                mkTest [] []
                    "{{ -14.1|truncate }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTest [] []
                    "{{ -14.8|truncate }}"
                    "-14"
            ]
        , testCase "\"urlencode\"" $ do
            mkTest [] []
                "{{ \"a/b c\"|urlencode }}"
                "a%2Fb%20c"
        , testGroup "\"sort\""
            [ testCase "simple" $ do
                mkTest [] []
                    "{{ [2,3,1]|sort }}"
                    "123"
            , testCase "reverse" $ do
                mkTest [] []
                    "{{ [2,3,1]|sort(reverse=true) }}"
                    "321"
            , testCase "sort by key" $ do
                mkTest [] []
                    "{{ [ {\"age\":30, \"name\":\"zzz\"}, {\"age\":41, \"name\":\"aaa\"} ]|sort(by=\"name\") }}"
                    "41aaa30zzz"
            , testCase "sort by key, reverse" $ do
                mkTest [] []
                    "{{ [ {\"age\":30, \"name\":\"zzz\"}, {\"age\":41, \"name\":\"aaa\"} ]|sort(by=\"age\", reverse=true) }}"
                    "41aaa30zzz"
            ]
        , testGroup "\"slice\""
            [ testCase "full positional args" $ do
                mkTest [] []
                    "{{ [1, 2, 3, 4, 5]|slice(1,3) }}"
                    "234"
            , testCase "implicit 'to end'" $ do
                mkTest [] []
                    "{{ [1, 2, 3, 4, 5]|slice(1) }}"
                    "2345"
            , testCase "full named args" $ do
                mkTest [] []
                    "{{ [1, 2, 3, 4, 5]|slice(length=3,start=1) }}"
                    "234"
            ]
        ]
    , testGroup "Setting variables"
        [ testCase "plain" $ do
            mkTest [] []
                "{% set x = \"world\" %}Hello, {{ x }}!"
                "Hello, world!"
        , testCase "self-referential" $ do
            mkTest [] []
                "{% set x = { \"foo\":\"bar\" } %}{% set x = x[\"foo\"] %}Hello, {{ x }}!"
                "Hello, bar!"
        ]
    , testGroup "HTML encoding"
        [ testCase "no encoding outside of constructs" $ do
            mkTest [] []
                "<foo bar=\"baz\"/>Ampersand is '&'</foo>."
                "<foo bar=\"baz\"/>Ampersand is '&'</foo>."
        , testCase "auto-encode inside interpolations" $ do
            mkTest [] []
                "{{ \"<foo bar=\\\"baz\\\"/>amp is '&'</foo>.\" }}"
                "&lt;foo bar=&quot;baz&quot;/&gt;amp is &apos;&amp;&apos;&lt;/foo&gt;."
        , testCase "raw filter bypasses encoding" $ do
            mkTest [] []
                "{{ \"<a>&amp;\"|raw }}"
                "<a>&amp;"
        ]
    , testGroup "Includes"
        [ testCase "include plain" $ do
            mkTest [] [("./features-included.html", "This is an included template")]
                "{% include 'features-included.html' %}"
                "This is an included template"
        , testCase "include with a variable" $ do
            mkTest [] [("./features-included.html", "Hello, {{ user }}!")]
                "{% set user='world' %}{% include 'features-included.html' %}"
                "Hello, world!"
        , testCase "include referencing an included variable" $ do
            mkTest [] [("./features-included.html", "{% set user = 'foobar' %}")]
                "{% include 'features-included.html' %}Hello, {{ user }}!"
                "Hello, foobar!"
        ]
    , testGroup "Explicit Local Scopes"
        [ testCase "baseline" $ do
            mkTest [] [] "{% set bedazzle = \"no\" %}{{ bedazzle }}" "no"
        , testCase "inside local scope" $ do
            mkTest [] [] "{% set bedazzle = \"no\" %}{% scope %}{% set bedazzle = \"ya\" %}{{ bedazzle }}{% endscope %}" "ya"
        , testCase "after exiting local scope" $ do
            mkTest [] [] "{% set bedazzle = \"no\" %}{% scope %}{% set bedazzle = \"ya\" %}{% endscope %}{{ bedazzle }}" "no"
        ]
    , testGroup "Macros"
        [ testCase "simple" $ do
            mkTest [] []
                "{% macro foobar -%}baz{%- endmacro %}{{ foobar() }}"
                "baz"
        , testCase "with args" $ do
            mkTest [] []
                "{% macro foobar2(baz) -%}{{ baz }}oo{%- endmacro %}{{ foobar2(boink=\"nope\", baz=\"blabber\") }}"
                "blabberoo"
        ]
    , testGroup "Lambdas"
        [ testCase "single arg" $ do
            mkTest [] [] "{{ ((x) -> x * x)(2) }}" "4"
        , testCase "two args" $ do
            mkTest [] [] "{{ ((greeting, name) -> greeting ~ \", \" ~ name ~ \"!\")(\"Hello\", \"world\") }}" "Hello, world!"
        ]
    , testGroup "Ternary operator"
        [ testCase "C syntax" $ do
            mkTest [] [] "{{ 1 ? \"yes\" : \"no\" }}" "yes"
        , testCase "python syntax" $ do
            mkTest [] [] "{{ \"yes\" if 1 else \"no\" }}" "yes"
        , testCase "C syntax, nested, true/false" $ do
            mkTest [] [] "{{ true ? false ? \"a\" : \"b\" : \"c\" }}" "b"
        , testCase "Python syntax, nested, true/false" $ do
            mkTest [] [] "{{ \"a\" if false else \"b\" if true else \"c\" }}" "b"
        , testCase "C syntax, nested, true/true" $ do
            mkTest [] [] "{{ true ? true ? \"a\" : \"b\" : \"c\" }}" "a"
        , testCase "Python syntax, nested, true/true" $ do
            mkTest [] [] "{{ false ? true ? \"a\" : \"b\" : \"c\" }}" "c"
        ]
    , testGroup "Call syntax"
        [ testCase "\"caller\"" $ do
            mkTest [] [] "{% macro foobar3(a) -%}{{ a }}({{ caller(\"asdf\") }}){%- endmacro %}{% call (a) foobar3(\"hey\") %}<{{a}}>{% endcall %}" "hey(<asdf>)"
        ]
    , testGroup "Inheritance"
        [ testCase "inheritance" $ do
            mkTest
                []
                [ ("./inherit-child.html", "{% extends \"inherit-parent.html\" %}{% block test -%}This is right.{% endblock %}")
                , ("./inherit-parent.html", "{%- block test -%}This is wrong.{% endblock -%}")
                ]
                "{% include \"inherit-child.html\" %}"
                "This is right."
        ]
    ]

mkTest :: [(VarName, GVal (Run IO Html))] -> [(FilePath, String)] -> String -> Text -> Assertion
mkTest contextDict includeLookup src expected = do
    let resolver srcName = return $ lookup srcName includeLookup
    template <- either (fail . show) return =<< parseGinger resolver Nothing src
    output <- newIORef mempty
    let write h = modifyIORef output (<> h)
    let context = makeContextHtmlM
                    (\key -> return $ fromMaybe def (lookup key contextDict))
                    write
    runGingerT context template
    actual <- htmlSource <$> readIORef output
    assertEqual "" expected actual
