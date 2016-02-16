#!/bin/bash
OUTPUT=$(ginger templates/features.html)
echo "$OUTPUT" | grep -A3 '^templates/'
EXPECT=$(echo "$OUTPUT" | grep '^expect:')
ACTUAL=$(echo "$OUTPUT" | grep '^actual:')
diff -i -u <(echo "$EXPECT" | sed 's/^expect://') <(echo "$ACTUAL" | sed 's/^actual://')
