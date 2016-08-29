{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
module Text.Ginger.SimulationTests
where

import Text.Ginger
import Text.Ginger.Html
import Test.Tasty
import Test.Tasty.HUnit
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Monoid
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.Lazy as LBS
import Data.Time (TimeLocale)

simulationTests :: TestTree
simulationTests = testGroup "Simulation"
    [ testCase "Smoke Test" $ mkTestHtml [] [] "Hello" "Hello"
    , testGroup "Comments"
        -- There is a comment between the two dashes that should not appear in
        -- the output.
        [ testCase "Comment does not appear in output" $ mkTestHtml
            [] [] "- {# Comments #} -" "-  -"
        ]
    , testGroup "Dashed limiters eat whitespace"
        [ testCase "comments" $ mkTestHtml
            [] [] "- {#- Comment -#} -" "--"
        , testCase "interpolations" $ mkTestHtml
            [] [] "- {{- '' -}} -" "--"
        , testCase "flow" $ mkTestHtml
            [] [] "- {%- set x=1 -%} -" "--"
        ]
    , testGroup "Literals"
        [ testCase "String: \"foobar\"" $ mkTestHtml
            [] [] "{{ \"foobar\" }}" "foobar"
        , testGroup "Numbers"
            [ testCase "123" $ mkTestHtml
                [] [] "{{ 123 }}" "123"
            , testCase "3.1415" $ mkTestHtml
                [] [] "{{ 3.1415 }}" "3.1415"
            ]
        , testGroup "Booleans"
            [ testCase "true" $ mkTestHtml
                [] [] "{{ true }}" "1"
            , testCase "false" $ mkTestHtml
                [] [] "{{ false }}" ""
            ]
        , testCase "Null" $ mkTestHtml
            [] [] "{{ null }}" ""
        ]
    , testGroup "Simple list/object constructs"
        [ testCase "Lists" $ mkTestHtml
            [] [] "{{ [\"foo\",\"bar\",\"baz\" ] }}" "foobarbaz"
        , testCase "Nested lists" $ mkTestHtml
            [] [] "{{ [ \"foo\", \"bar\", [ 1, 2, 3 ], \"baz\" ] }}" "foobar123baz"
        , testCase "Objects" $ mkTestHtml
            [] [] "{{ { \"foo\":\"bar\" } }}" "bar"
        , testCase "Nested object/list constructs" $ mkTestHtml
            [] [] "{{ { \"foo\":[\"foo\", {\"asdf\" : \"bar\"}, [\"baz\" ]] } }}" "foobarbaz"
        ]
    , testGroup "Accessing object/list members"
        [ testCase "by integer index" $ mkTestHtml
            [] [] "{{ [ \"foo\", \"bar\" ][1] }}" "bar"
        , testCase "by string key" $ mkTestHtml
            [] [] "{{ { \"foo\": \"bar\", \"baz\": \"quux\" }['foo'] }}" "bar"
        , testCase "by property name" $ mkTestHtml
            [] [] "{{ { \"foo\": \"bar\", \"baz\": \"quux\" }.foo }}" "bar"
        , testCase "multi-level mixed" $ mkTestHtml
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
            mkTestHtml [("print", fromFunction printF)] [] "{{ print(\"Hello\") }}" ""
            actual <- readIORef buf
            let expected = "Hello"
            assertEqual "" actual expected
        , testCase "\"Hello\"|print" $ do
            buf <- newIORef ""
            let printF :: Function (Run IO Html)
                printF = \xs -> do
                    liftIO . writeIORef buf . mconcat . map (asText . snd) $ xs
                    return def
            mkTestHtml [("print", fromFunction printF)] [] "{{ \"Hello\"|print }}" ""
            actual <- readIORef buf
            let expected = "Hello"
            assertEqual "" actual expected
        ]
    , testGroup "Addition"
        [ testCase "1 + 1 = 2" $ do
            mkTestHtml [] [] "{{ sum(1, 1) }}" "2"
        , testCase "1 + 1 = 2" $ do
            mkTestHtml [] [] "{{ 1 + 1 }}" "2"
        ]
    , testGroup "Subtraction"
        [ testCase "1 - 1 = 0" $ do
            mkTestHtml [] [] "{{ 1 - 1 }}" "0"
        ]
    , testGroup "Concatenation"
        [ testCase "1 ~ \"foo\" = 1foo" $ do
            mkTestHtml [] [] "{{ 1 ~ \"foo\" }}" "1foo"
        ]
    , testGroup "Multiplication"
        [ testCase "5 * 5 = 25" $ do
            mkTestHtml [] [] "{{ 5 * 5 }}" "25"
        ]
    , testGroup "Division"
        [ testCase "24 / 6 = 4" $ do
            mkTestHtml [] [] "{{ 24 / 6 }}" "4"
        , testCase "3 / 2 = 1.5" $ do
            mkTestHtml [] [] "{{ 3 / 2 }}" "1.5"
        ]
    , testGroup "Integer Division"
        [ testCase "24 // 6 = 4" $ do
            mkTestHtml [] [] "{{ 24 // 6 }}" "4"
        , testCase "3 // 2 = 1" $ do
            mkTestHtml [] [] "{{ 3 // 2 }}" "1"
        ]
    , testGroup "Modulo"
        [ testCase "7 % 3 = 2" $ do
            mkTestHtml [] [] "{{ 7 % 3 }}" "1"
        ]
    , testGroup "Iteration"
        [ testCase "for x in [ \"foo\", \"bar\", \"baz\" ]: <x>" $ do
            mkTestHtml [] [] "{% for x in [ \"foo\", \"bar\", \"baz\" ] %}<{{x}}>{% endfor %}" "<foo><bar><baz>"
        , testCase "for x in []: <x> else <no>" $ do
            mkTestHtml [] [] "{% for x in [] %}<{{x}}>{% else %}<no>{% endfor %}" "<no>"
        , testCase "for x in [a]: <x> else <no>" $ do
            mkTestHtml [] [] "{% for x in [\"a\"] %}<{{x}}>{% else %}<no>{% endfor %}" "<a>"
        ]
    , testGroup "The `loop` auto-variable"
        [ testCase "loop.cycle" $ do
            mkTestHtml [] []
                ( "{% for x in [\"foo\", \"bar\", \"baz\"] recursive -%}" ++
                  "({{ loop.cycle(\"red\", \"green\") }}/{{ x }})" ++
                  "{%- endfor %}"
                )
                "(red/foo)(green/bar)(red/baz)"
        , testCase "recursive loops" $ do
            mkTestHtml [] []
                ( "{% for k, x in {\"a\":{\"b\":null,\"c\":{\"d\":null}}} -%}" ++
                  "{{ k }}@{{ loop.depth }}({{ loop(x) }})" ++
                  "{%- endfor %}" :: String
                )
                "a@1(b@2()c@2(d@3()))"
        ]
    , testGroup "Conditionals"
        [ testCase "if true then \"yes\" else \"no\"" $ do
            mkTestHtml [] [] "{% if true %}yes{% else %}no{% endif %}" "yes"
        , testCase "if false then \"yes\" else if false then \"maybe\" else \"no\"" $ do
            mkTestHtml [] [] "{% if false %}yes{% elif false %}maybe{% else %}no{% endif %}" "no"
        , testCase "if false then \"yes\" else if true then \"maybe\" else \"no\"" $ do
            mkTestHtml [] [] "{% if false %}yes{% elif true %}maybe{% else %}no{% endif %}" "maybe"
        ]
    , testGroup "Comparisons"
        [ testCase "if 1 == 1 then \"yes\" else \"no\"" $ do
            mkTestHtml [] [] "{% if (1 == 1) %}yes{% else %}no{% endif %}" "yes"
        , testCase "if 1 > 0 then \"yes\" else \"no\"" $ do
            mkTestHtml [] [] "{% if (1 > 0) %}yes{% else %}no{% endif %}" "yes"
        , testCase "if 1 > null then \"yes\" else \"no\"" $ do
            mkTestHtml [] [] "{% if (1 > null) %}yes{% else %}no{% endif %}" "no"
        , testCase "if 1 < 2 then \"yes\" else \"no\"" $ do
            mkTestHtml [] [] "{% if (1 < 2) %}yes{% else %}no{% endif %}" "yes"
        , testCase "if null < 1 then \"yes\" else \"no\"" $ do
            mkTestHtml [] [] "{% if (null < 1) %}yes{% else %}no{% endif %}" "no"
        ]
    , testGroup "Boolean AND"
        [ testCase "AND (both)" $ do
            mkTestHtml [] [] "{% if 1 && 2 %}yes{% else %}no{% endif %}" "yes"
        , testCase "AND (only one)" $ do
            mkTestHtml [] [] "{% if 1 && 0 %}yes{% else %}no{% endif %}" "no"
        , testCase "AND (neither)" $ do
            mkTestHtml [] [] "{% if 0 && 0 %}yes{% else %}no{% endif %}" "no"
        ]
    , testGroup "Boolean AND"
        [ testCase "OR (both)" $ do
            mkTestHtml [] [] "{% if 1 || 2 %}yes{% else %}no{% endif %}" "yes"
        , testCase "OR (only one)" $ do
            mkTestHtml [] [] "{% if 1 || 0 %}yes{% else %}no{% endif %}" "yes"
        , testCase "OR (either)" $ do
            mkTestHtml [] [] "{% if 0 || 0 %}yes{% else %}no{% endif %}" "no"
        ]
    , testGroup "Slicing brackets"
        [ testCase "from/to, both positive" $ do
            mkTestHtml [] []
                "{{ 'abcdef'[1:2] }}"
                "bc"
        , testCase "from/to, from negative" $ do
            mkTestHtml [] []
                "{{ 'abcdef'[-3:2] }}"
                "de"
        , testCase "from/to, to implicit" $ do
            mkTestHtml [] []
                "{{ 'abcdef'[1:] }}"
                "bcdef"
        , testCase "from/to, from implicit" $ do
            mkTestHtml [] []
                "{{ 'abcdef'[:2] }}"
                "ab"
        ]
    , testGroup "Built-in filters/functions"
        [ testCase "\"abs\"" $ do
            mkTestHtml [] [] "{{ -2|abs }}" "2"
        , testGroup "\"any\""
            [ testCase "\"any\" (both)" $ do
                mkTestHtml [] [] "{% if any(1, 1, true) %}yes{% else %}no{% endif %}" "yes"
            , testCase "\"any\" (just one)" $ do
                mkTestHtml [] []
                    "{% if any(0, 1, false) %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "\"any\" (neither)" $ do
                mkTestHtml [] []
                    "{% if any(0, 0, false) %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"all\""
            [ testCase "\"all\" (both)" $ do
                mkTestHtml [] []
                    "{% if all(1, 1, true) %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "\"all\" (just one)" $ do
                mkTestHtml [] []
                    "{% if all(0, 1, false) %}yes{% else %}no{% endif %}"
                    "no"
            , testCase "\"all\" (neither)" $ do
                mkTestHtml [] []
                    "{% if all(0, 0, false) %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"ceil\""
            [ testCase "14.1" $ do
                mkTestHtml [] []
                    "{{ 14.1|ceil }}"
                    "15"
            , testCase "-14.1" $ do
                mkTestHtml [] []
                    "{{ -14.1|ceil }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTestHtml [] []
                    "{{ -14.8|ceil }}"
                    "-14"
            ]
        , testCase "\"capitalize\"" $ do
            mkTestHtml [] []
                "{{ \"this is the end of the world\"|capitalize }}"
                "This is the end of the world"
        , testGroup "\"center\""
            [ testCase "extra space" $ do
                mkTestHtml [] []
                    "{{ \"asdf\"|center(12) }}"
                    "    asdf    "
            , testCase "no extra space" $ do
                mkTestHtml [] []
                    "{{ \"foobar\"|center(2) }}"
                    "foobar"
            ]
        , testCase "\"concat\"" $ do
            mkTestHtml [] []
                "{{ [\"hello\", \"world\"]|concat }}"
                "helloworld"
        , testGroup "\"contains\""
            [ testCase "single match" $ do
                mkTestHtml [] []
                    "{% if ['hello', 'world']|contains('hello') %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "multi match" $ do
                mkTestHtml [] []
                    "{% if ['hello', 'world']|contains('hello', 'world') %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "non-match" $ do
                mkTestHtml [] []
                    "{% if ['hello', 'world']|contains('hello', 'you', 'world') %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"date\""
            [ testCase "format a date" $ do
                mkTestHtml [] []
                    "{{ {'year':2015, 'month':6, 'day':13}|date('%Y-%m-%d') }}"
                    "2015-06-13"
            , testCase "format a list as a date" $ do
                mkTestHtml [] []
                    "{{ [2015, 6, 13, 12, 45, 21]|date('%Y-%m-%d') }}"
                    "2015-06-13"
            , testCase "format a 5-element list as a date" $ do
                mkTestHtml [] []
                    "{{ [2015, 6, 13, 12, 45]|date('%Y-%m-%d') }}"
                    "2015-06-13"
            , testCase "format a 3-element list as a date" $ do
                mkTestHtml [] []
                    "{{ [2015, 6, 13]|date('%Y-%m-%d') }}"
                    "2015-06-13"
            , testCase "use correct default time (noon) with 3-element list" $ do
                mkTestHtml [] []
                    "{{ [2015, 6, 13]|date('%H:%M:%S') }}"
                    "12:00:00"
            , testCase "format a string as a date" $ do
                mkTestHtml [] []
                    "{{ '2015-06-13 12:05:43'|date('%Y-%m-%d') }}"
                    "2015-06-13"
            , testCase "format a string as a date (JSON-style formatting)" $ do
                mkTestHtml [] []
                    "{{ '2015-06-13T12:05:43Z'|date('%Y-%m-%d %H:%M:%S') }}"
                    "2015-06-13 12:05:43"
            , testCase "format a string as a time-of-day" $ do
                mkTestHtml [] []
                    "{{ '2015-06-13 12:05:43'|date('%H-%M-%S') }}"
                    "12-05-43"
            , testCase "format a string as a date, with timezone" $ do
                mkTestHtml [] []
                    "{{ '2015-06-13 12:05:43-05:00'|date('%Y-%m-%d %z') }}"
                    "2015-06-13 -0500"
            , testCase "format a local-time string as a date + time, with explicit timezone" $ do
                mkTestHtml [] []
                    "{{ '2015-06-13 12:05:43'|date('%Y-%m-%d %H:%M %z', tz='+0200') }}"
                    "2015-06-13 12:05 +0200"
            , testCase "format a zoned-time string as a date, with explicit timezone" $ do
                mkTestHtml [] []
                    "{{ '2015-06-13 12:05:43-05:00'|date('%Y-%m-%d %H:%M %z', tz='+0200') }}"
                    "2015-06-13 19:05 +0200"
            , testCase "use a custom locale" $ do
                sillyLocale <- loadSillyLocale
                mkTestHtml
                    [("silly", toGVal (sillyLocale :: Maybe JSON.Value))]
                    []
                    "{{ '2015-06-13 12:05:43'|date('%c', locale=silly) }}"
                    "It be The Day Of Saturn, in the year 15, the clock striketh 12"
            ]
        , testGroup "\"default\""
            [ testCase "trigger default" $ do
                mkTestHtml [] []
                    "{{ 0|default(\"hi\") }}"
                    "hi"
            , testCase "use truthy value" $ do
                mkTestHtml [] []
                    "{{ \"hi\"|default(\"nope\") }}"
                    "hi"
            ]
        , testCase "\"difference\"" $ do
            mkTestHtml [] []
                "{{ difference(5,2) }}"
                "3"
        , testGroup "\"dictsort\""
            [ testCase "by key" $ do
                mkTestHtml [] []
                    "{{ dictsort({4:4, 1:5}, by='key') }}"
                    "54"
            , testCase "by value" $ do
                mkTestHtml [] []
                    "{{ dictsort({4:4, 1:5}, by='value') }}"
                    "45"
            ]
        , testGroup "\"escape\""
            [ testCase "single item" $ do
                mkTestHtml [] []
                    "{{ escape('<')|raw }}"
                    "&lt;"
            , testCase "multiple items" $ do
                mkTestHtml [] []
                    "{{ escape('<', '>')|raw }}"
                    "&lt;&gt;"
            ]
        , testGroup "\"equals\""
            [ testCase "all equal" $ do
                mkTestHtml [] []
                    "{% if equals(1, 1, 1) %}yes{% else %}no{% endif %}"
                    "yes"
            , testCase "some equal" $ do
                mkTestHtml [] []
                    "{% if equals(1, 1, 2) %}yes{% else %}no{% endif %}"
                    "no"
            ]
        , testGroup "\"filesizeformat\""
            [ testCase "bytes" $ do
                mkTestHtml [] []
                    "{{ 100|filesizeformat }}"
                    "100 B"
            , testCase "kilobytes" $ do
                mkTestHtml [] []
                    "{{ 12000|filesizeformat }}"
                    "12 kB"
            , testCase "megabytes" $ do
                mkTestHtml [] []
                    "{{ 1500000|filesizeformat }}"
                    "1.5 MB"
            , testCase "bytes (2-based)" $ do
                mkTestHtml [] []
                    "{{ 100|filesizeformat(true) }}"
                    "100 B"
            , testCase "kibibytes" $ do
                mkTestHtml [] []
                    "{{ 12000|filesizeformat(true) }}"
                    "11.7 kiB"
            , testCase "mebibytes" $ do
                mkTestHtml [] []
                    "{{ 1500000|filesizeformat(true) }}"
                    "1.4 MiB"
            ]
        , testGroup "\"filter\""
            [ testCase "simple case" $ do
                mkTestHtml [] []
                    "{{ [1, 0, 3]|filter(int) }}"
                    "13"
            , testCase "with extra argument" $ do
                mkTestHtml [] []
                    "{{ [1, 2, 3]|filter(greater, 2) }}"
                    "3"
            ]
        , testGroup "\"format\""
            [ testCase "jinja.pocoo.org example" $ do
                mkTestHtml [] []
                    "{{ \"%s - %s\"|format('Hello?', 'Foo!') }}"
                    "Hello? - Foo!"
            ]
        , testGroup "\"not-equals\""
            [ testCase "all equal" $ do
                mkTestHtml [] []
                    "{% if nequals(1, 1, 1) %}yes{% else %}no{% endif %}"
                    "no"
            , testCase "not all equal" $ do
                mkTestHtml [] []
                    "{% if nequals(1, 1, 2) %}yes{% else %}no{% endif %}"
                    "yes"
            ]
        , testGroup "\"floor\""
            [ testCase "14.1" $ do
                mkTestHtml [] []
                    "{{ 14.1|floor }}"
                    "14"
            , testCase "14.8" $ do
                mkTestHtml [] []
                    "{{ 14.8|floor }}"
                    "14"
            , testCase "-14.1" $ do
                mkTestHtml [] []
                    "{{ -14.1|floor }}"
                    "-15"
            , testCase "-14.8" $ do
                mkTestHtml [] []
                    "{{ -14.8|floor }}"
                    "-15"
            ]
        , testGroup "\"int\""
            [ testCase "14.1" $ do
                mkTestHtml [] []
                    "{{ 14.1|int }}"
                    "14"
            , testCase "14.8" $ do
                mkTestHtml [] []
                    "{{ 14.8|int }}"
                    "14"
            , testCase "-14.1" $ do
                mkTestHtml [] []
                    "{{ -14.1|int }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTestHtml [] []
                    "{{ -14.8|int }}"
                    "-14"
            ]
        -- \"int_ratio\"
        -- TODO
        -- \"iterable\"
        -- TODO
        , testCase "\"length\"" $ do
            mkTestHtml [] []
                "{{ [1,2,3]|length }}"
                "3"
        -- \"modulo\"
        -- \"num\"
        -- TODO
        -- \"printf\"
        , testGroup "\"printf\""
            [ testCase "%s, passed as int" $ do
                mkTestHtml [] []
                    "{{ printf(\"%i\", 1) }}"
                    "1"
            , testCase "%s, passed as string" $ do
                mkTestHtml [] []
                    "{{ printf(\"%i\", \"1\") }}"
                    "1"
            , testCase "%i, passed as float" $ do
                mkTestHtml [] []
                    "{{ printf(\"%i\", 1.3) }}"
                    "1"
            , testCase "%f, passed as int" $ do
                mkTestHtml [] []
                    "{{ printf(\"%f\", 1) }}"
                    "1.0"
            , testCase "%.3f, passed as int" $ do
                mkTestHtml [] []
                    "{{ printf(\"%.3f\", 1) }}"
                    "1.000"
            , testCase "%s, string" $ do
                mkTestHtml [] []
                    "{{ printf(\"%s\", \"Hello\") }}"
                    "Hello"
            ]
        , testCase "\"product\"" $ do
            mkTestHtml [] []
                "{{ product(1,2,3) }}"
                "6"
        , testCase "\"ratio\"" $ do
            mkTestHtml [] []
                "{{ ratio(6, 1.5, 2) }}"
                "2"
        , testGroup "\"round\""
            [ testCase "14.1" $ do
                mkTestHtml [] []
                    "{{ 14.1|round }}"
                    "14"
            , testCase "14.8" $ do
                mkTestHtml [] []
                    "{{ 14.8|round }}"
                    "15"
            , testCase "-14.1" $ do
                mkTestHtml [] []
                    "{{ -14.1|round }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTestHtml [] []
                    "{{ -14.8|round }}"
                    "-15"
            ]
        -- \"show\"
        -- TODO
        , testCase "\"str\"" $ do
            mkTestHtml [] []
                "{{ str(123) }}"
                "123"
        , testCase "\"sum\"" $ do
            mkTestHtml [] []
                "{{sum(1, 2, 3)}}"
                "6"
        , testGroup "\"truncate\""
            [ testCase "14.1" $ do
                mkTestHtml [] []
                    "{{ 14.1|truncate }}"
                    "14"
            , testCase "14.8" $ do
                mkTestHtml [] []
                    "{{ 14.8|truncate }}"
                    "14"
            , testCase "-14.1" $ do
                mkTestHtml [] []
                    "{{ -14.1|truncate }}"
                    "-14"
            , testCase "-14.8" $ do
                mkTestHtml [] []
                    "{{ -14.8|truncate }}"
                    "-14"
            ]
        , testCase "\"urlencode\"" $ do
            mkTestHtml [] []
                "{{ \"a/b c\"|urlencode }}"
                "a%2Fb%20c"
        , testGroup "\"sort\""
            [ testCase "simple" $ do
                mkTestHtml [] []
                    "{{ [2,3,1]|sort }}"
                    "123"
            , testCase "reverse" $ do
                mkTestHtml [] []
                    "{{ [2,3,1]|sort(reverse=true) }}"
                    "321"
            , testCase "sort by key" $ do
                mkTestHtml [] []
                    "{{ ([ {\"age\":30, \"name\":\"zzz\"}, {\"age\":41, \"name\":\"aaa\"} ]|sort(by=\"name\"))[0]['name'] }}"
                    "aaa"
            , testCase "sort by key, reverse" $ do
                mkTestHtml [] []
                    "{{ ([ {\"age\":30, \"name\":\"zzz\"}, {\"age\":41, \"name\":\"aaa\"} ]|sort(by=\"age\", reverse=true))[0]['name'] }}"
                    "aaa"
            ]
        , testGroup "\"slice\""
            [ testCase "full positional args" $ do
                mkTestHtml [] []
                    "{{ [1, 2, 3, 4, 5]|slice(1,3) }}"
                    "234"
            , testCase "implicit 'to end'" $ do
                mkTestHtml [] []
                    "{{ [1, 2, 3, 4, 5]|slice(1) }}"
                    "2345"
            , testCase "full named args" $ do
                mkTestHtml [] []
                    "{{ [1, 2, 3, 4, 5]|slice(length=3,start=1) }}"
                    "234"
            , testCase "negative offset" $ do
                mkTestHtml [] []
                    "{{ [1, 2, 3, 4, 5]|slice(start=-1) }}"
                    "5"
            , testCase "call on string subject" $ do
                mkTestHtml [] []
                    "{{ \"12345\"|slice(1,3) }}"
                    "234"
            ]
        , testGroup "\"replace\""
            [ testCase "simple case" $ do
                mkTestHtml [] []
                    "{{ replace('foobar', 'a', 'e') }}"
                    "foober"
            , testCase "multiple replacements" $ do
                mkTestHtml [] []
                    "{{ replace('foobar', 'o', 'e') }}"
                    "feebar"
            , testCase "longer replacements" $ do
                mkTestHtml [] []
                    "{{ replace('foobar', 'oo', 'e') }}"
                    "febar"
            , testCase "deletion" $ do
                mkTestHtml [] []
                    "{{ replace('foobar', 'o') }}"
                    "fbar"
            ]
        ]
    , testGroup "Setting variables"
        [ testCase "plain" $ do
            mkTestHtml [] []
                "{% set x = \"world\" %}Hello, {{ x }}!"
                "Hello, world!"
        , testCase "self-referential" $ do
            mkTestHtml [] []
                "{% set x = { \"foo\":\"bar\" } %}{% set x = x[\"foo\"] %}Hello, {{ x }}!"
                "Hello, bar!"
        ]
    , testGroup "HTML encoding"
        [ testCase "no encoding outside of constructs" $ do
            mkTestHtml [] []
                "<foo bar=\"baz\"/>Ampersand is '&'</foo>."
                "<foo bar=\"baz\"/>Ampersand is '&'</foo>."
        , testCase "auto-encode inside interpolations" $ do
            mkTestHtml [] []
                "{{ \"<foo bar=\\\"baz\\\"/>amp is '&'</foo>.\" }}"
                "&lt;foo bar=&quot;baz&quot;/&gt;amp is &apos;&amp;&apos;&lt;/foo&gt;."
        , testCase "raw filter bypasses encoding" $ do
            mkTestHtml [] []
                "{{ \"<a>&amp;\"|raw }}"
                "<a>&amp;"
        ]
    , testGroup "Includes"
        [ testCase "include plain" $ do
            mkTestHtml [] [("./features-included.html", "This is an included template")]
                "{% include 'features-included.html' %}"
                "This is an included template"
        , testCase "include with a variable" $ do
            mkTestHtml [] [("./features-included.html", "Hello, {{ user }}!")]
                "{% set user='world' %}{% include 'features-included.html' %}"
                "Hello, world!"
        , testCase "include referencing an included variable" $ do
            mkTestHtml [] [("./features-included.html", "{% set user = 'foobar' %}")]
                "{% include 'features-included.html' %}Hello, {{ user }}!"
                "Hello, foobar!"
        ]
    , testGroup "Explicit Local Scopes"
        [ testCase "baseline" $ do
            mkTestHtml [] [] "{% set bedazzle = \"no\" %}{{ bedazzle }}" "no"
        , testCase "inside local scope" $ do
            mkTestHtml [] [] "{% set bedazzle = \"no\" %}{% scope %}{% set bedazzle = \"ya\" %}{{ bedazzle }}{% endscope %}" "ya"
        , testCase "after exiting local scope" $ do
            mkTestHtml [] [] "{% set bedazzle = \"no\" %}{% scope %}{% set bedazzle = \"ya\" %}{% endscope %}{{ bedazzle }}" "no"
        ]
    , testGroup "Macros"
        [ testCase "simple" $ do
            mkTestHtml [] []
                "{% macro foobar -%}baz{%- endmacro %}{{ foobar() }}"
                "baz"
        , testCase "with args" $ do
            mkTestHtml [] []
                "{% macro foobar2(baz) -%}{{ baz }}oo{%- endmacro %}{{ foobar2(boink=\"nope\", baz=\"blabber\") }}"
                "blabberoo"
        ]
    , testGroup "Lambdas"
        [ testCase "single arg" $ do
            mkTestHtml [] [] "{{ ((x) -> x * x)(2) }}" "4"
        , testCase "two args" $ do
            mkTestHtml [] [] "{{ ((greeting, name) -> greeting ~ \", \" ~ name ~ \"!\")(\"Hello\", \"world\") }}" "Hello, world!"
        ]
    , testGroup "Ternary operator"
        [ testCase "C syntax" $ do
            mkTestHtml [] [] "{{ 1 ? \"yes\" : \"no\" }}" "yes"
        , testCase "python syntax" $ do
            mkTestHtml [] [] "{{ \"yes\" if 1 else \"no\" }}" "yes"
        , testCase "C syntax, nested, true/false" $ do
            mkTestHtml [] [] "{{ true ? false ? \"a\" : \"b\" : \"c\" }}" "b"
        , testCase "Python syntax, nested, true/false" $ do
            mkTestHtml [] [] "{{ \"a\" if false else \"b\" if true else \"c\" }}" "b"
        , testCase "C syntax, nested, true/true" $ do
            mkTestHtml [] [] "{{ true ? true ? \"a\" : \"b\" : \"c\" }}" "a"
        , testCase "Python syntax, nested, true/true" $ do
            mkTestHtml [] [] "{{ false ? true ? \"a\" : \"b\" : \"c\" }}" "c"
        ]
    , testGroup "Call syntax"
        [ testCase "\"caller\"" $ do
            mkTestHtml [] [] "{% macro foobar3(a) -%}{{ a }}({{ caller(\"asdf\") }}){%- endmacro %}{% call (a) foobar3(\"hey\") %}<{{a}}>{% endcall %}" "hey(<asdf>)"
        ]
    , testGroup "Inheritance"
        [ testCase "inheritance" $ do
            mkTestHtml
                []
                [ ("./inherit-child.html", "{% extends \"inherit-parent.html\" %}{% block test -%}This is right.{% endblock %}")
                , ("./inherit-parent.html", "{%- block test -%}This is wrong.{% endblock -%}")
                ]
                "{% include \"inherit-child.html\" %}"
                "This is right."
        ]
    , testGroup "Non-HTML Output"
        [ testCase "text" $ do
            mkTestText
                []
                []
                "<>{{ '<>' }}<>"
                "<><><>"
        , testCase "json" $ do
            mkTestJSON
                []
                []
                "{{ '<>' }}{{ 1 }}{{ {'foo': true} }}"
                "[\"<>\",1,{\"foo\":true}]"
        ]
    ]

mkTestHtml :: [(VarName, GVal (Run IO Html))]
           -> [(FilePath, String)]
           -> String
           -> Text
           -> Assertion
mkTestHtml = mkTest makeContextHtmlM htmlSource

mkTestText :: [(VarName, GVal (Run IO Text))]
           -> [(FilePath, String)]
           -> String
           -> Text
           -> Assertion
mkTestText = mkTest makeContextTextM id

mkTestJSON :: [(VarName, GVal (Run IO [JSON.Value]))]
           -> [(FilePath, String)]
           -> String
           -> Text
           -> Assertion
mkTestJSON = mkTest
    (\l w -> makeContextM' l w toJSONSingleton) encodeText
    where
        toJSONSingleton = (:[]) . JSON.toJSON

encodeText :: JSON.ToJSON a => a -> Text
encodeText = Text.pack . LUTF8.toString . JSON.encode

mkTest :: (Monoid a, ToGVal (Run IO a) a)
       => ((VarName -> Run IO a (GVal (Run IO a))) -> (a -> IO ()) -> GingerContext IO a) -- ^ mkContextM flavor
       -> (a -> Text) -- ^ Convert a to Text for output
       -> [(VarName, GVal (Run IO a))] -- ^ Context dictionary
       -> [(FilePath, String)] -- ^ Lookup table for includes
       -> String -- ^ Template source
       -> Text -- ^ Expected output
       -> Assertion
mkTest mContext valToText contextDict includeLookup src expected = do
    let resolver srcName = return $ lookup srcName includeLookup
    template <- either (fail . show) return =<< parseGinger resolver Nothing src
    output <- newIORef mempty
    let write h = modifyIORef output (<> h)
    let context = mContext
                    (\key -> return $ fromMaybe def (lookup key contextDict))
                    write
    runGingerT context (optimize template)
    actual <- valToText <$> readIORef output
    assertEqual "" expected actual

loadSillyLocale :: IO (Maybe JSON.Value)
loadSillyLocale = do
    JSON.decode <$> LBS.readFile "test/fixtures/silly-locale.json"
