#!/bin/bash
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

echo compiling yacc stripper
cc -o ys ys.c

echo stripping C out of the grammar
./ys <../cql.y >cql.txt

echo formatting for railroad tool
(echo "// @nolint"; \
 awk <cql.txt 'BEGIN {FS="\n"; RS=""} {gsub("\n","",$0); print }' | \
 sed -e 's/:/ ::= /' -e's/;$//' -e 's/  */ /g' -e 's/$/ /' | \
 sed 's/%prec UMINUS//' | \
 sed -f replacements.txt ) >cql_grammar.txt

echo "railroad diagram format in cql_grammar.txt (paste into https://www.bottlecaps.de/rr/ui)"

cat <<EOF >cql_grammar.md
## Appendix 2: CQL Grammar

What follows is taken from a grammar snapshot with the tree building rules removed.
It should give a fair sense of the syntax of CQL (but not semantic validation).

EOF

echo "Snapshot as of $(date)" >>cql_grammar.md

cat <<EOF >>cql_grammar.md

### Operators and Literals

These are in order of priority lowest to highest

EOF

echo '```' >>cql_grammar.md

# use whole word match on these small replacements LS, RS, GE, LE, NE, EQEQ
egrep "^%left|^%right|^%nonassoc" <../cql.y | \
  sed -e 's/%left //' \
      -e 's/%right //' \
      -e 's/%nonassoc //' \
      -e "s/[[:<:]]LS[[:>:]]/'<<'/" \
      -e "s/[[:<:]]RS[[:>:]]/'>>'/" \
      -e "s/[[:<:]]GE[[:>:]]/'>='/" \
      -e "s/[[:<:]]LE[[:>:]]/'<='/" \
      -e "s/[[:<:]]NE[[:>:]]/'<>'/" \
      -e "s/[[:<:]]NE_[[:>:]]/'!='/" \
      -e "s/[[:<:]]EQEQ[[:>:]]/'=='/" \
      >>cql_grammar.md

echo '```' >>cql_grammar.md

cat <<EOF  >>cql_grammar.md
NOTE: The above varies considerably from the C binding order!!!

Literals:
EOF

echo '```' >>cql_grammar.md
cat <<EOF  >>cql_grammar.md
ID  /* a name */
STRLIT /* a string literal in SQL format e.g. 'it''s sql' */
CSTRLIT /* a string literal in C format e.g. "hello, world\n" */
BLOBLIT /* a blob literal in SQL format e.g. x'12ab' */
INTLIT /* integer literal */
LONGLIT /* long integer literal */
REALLIT /* floating point literal */
EOF
echo '```' >>cql_grammar.md

echo "### Statement/Type Keywords" >>cql_grammar.md
echo '```' >>cql_grammar.md
grep "%token [^<]" <../cql.y | sed "s/%token //" >>cql_grammar.md
echo '```' >>cql_grammar.md

cat <<EOF  >>cql_grammar.md
### Rules

Note that in many cases the grammar is more generous than the overall language and errors have to be checked on top of this, often this is done on purpose because even when it's possible it might be very inconvenient to do checks with syntax.  For example the grammar cannot enforce non-duplicate ids in id lists, but it could enforce non-duplicate attributes in attribute lists.  It chooses to do neither as they are easily done with semantic validation.  Thus the grammar is not the final authority on what constitutes a valid program but it's a good start.
EOF
echo '```' >>cql_grammar.md
sed <cql.txt "s/$/ /" | sed -f replacements.txt >>cql_grammar.md
echo '```' >>cql_grammar.md

echo wiki format in cql_grammar.md

echo create grammar.js
./tree_sitter.py > grammar.js

echo cleanup
rm ys
rm cql.txt
