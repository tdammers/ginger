#!/bin/bash
BACKEND=native
OUTMODE=run
DIFFMODE=0
INFILE="$(dirname "$0")/templates/features.html"

for ARG in $@
do
    case "$ARG" in
        "-node")
            BACKEND=node
            OUTMODE=run
        ;;
        "-nodesrc")
            BACKEND=node
            OUTMODE=dump
        ;;
        "-diff")
            DIFFMODE=1
        ;;
        *)
            INFILE="$ARG"
        ;;
    esac
done

case "$BACKEND" in
    "native")
        OUTPUT=$(ginger "$INFILE")
        ;;
    "node")
        JS_SRC=$(ginger --js "$INFILE")
        JS_SRC="$JS_SRC
            var output = ''
            module.exports(
                function (a) {
                    switch (typeof(a)) {
                        case 'number':
                            if (!Number.isNaN(a))
                                output = output + String(a)
                            break;
                        default:
                            output = output + (a || '')
                            break;
                    }
                },
                function(a) { return a },
                {})
            console.log(output)"
        case $OUTMODE in
            run)
                OUTPUT=$(echo "$JS_SRC" | node)
                ;;
            dump)
                OUTPUT="$JS_SRC"
                ;;
            *)
                echo "Invalid output mode" >&2
                exit 1
                ;;
        esac
        ;;
    *)
        echo "Invalid backend" >&2
        exit 2
        ;;
esac

if [ $DIFFMODE == 1 ]
    then
        EXPECT=$(echo "$OUTPUT" | grep '^expect:')
        ACTUAL=$(echo "$OUTPUT" | grep '^actual:')
        diff -i -u <(echo "$EXPECT" | sed 's/^expect://') <(echo "$ACTUAL" | sed 's/^actual://')
    else
        echo "$OUTPUT"
fi
