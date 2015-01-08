#!/bin/bash
OUT=$(./.cabal-sandbox/bin/ginger templates/features.html)
echo "$OUT" | grep -A3 '^templates/'
EXPECT=$(echo "$OUT" | grep '^expect:')
ACTUAL=$(echo "$OUT" | grep '^actual:')
diff -i -u <(echo "$EXPECT" | sed 's/^expect://') <(echo "$ACTUAL" | sed 's/^actual://')
