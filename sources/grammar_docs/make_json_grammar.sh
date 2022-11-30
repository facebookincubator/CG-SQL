#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# exit when any command fails
set -e

# move to the location of the script for a fixed reference
DIR="$( dirname -- "$0"; )"
cd "${DIR}" || exit

echo compiling yacc stripper
(cd ..; make out/ys)

echo compiling replacements
(cd ..; make out/json_replacements)

echo stripping JSON grammar
../out/ys <../json_test/json_test.y | sed -e "/^  *$/d" | ../out/json_replacements | sed -e 's/  *$//' >json.txt

echo formatting for railroad tool

awk <json.txt 'BEGIN {FS="\n"; RS=""} {gsub("\n","",$0); print }' | \
 sed -e 's/:/ ::= /' -e's/;$//' -e 's/  */ /g'  -e 's/  *$//' | \
 grep -v '^BOOL_LITERAL' > json_grammar.txt

echo "railroad diagram format in json_grammar.txt (paste into https://www.bottlecaps.de/rr/ui)"

cat <<EOF >json_grammar.md
---
id: x5
title: "Appendix 5: JSON Schema Grammar"
sidebar_label: "Appendix 5: JSON Schema Grammar"
---
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is taken from the JSON validation grammar with the tree building rules removed.

EOF

echo "Snapshot as of $(date)" >>json_grammar.md

cat <<EOF  >>json_grammar.md

### Rules

EOF

echo '```' >>json_grammar.md
cat json.txt >>json_grammar.md
echo '```' >>json_grammar.md

echo wiki format in json_grammar.md

echo cleanup
rm json.txt
