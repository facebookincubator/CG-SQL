
#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

RR=~/rr-1.63-java8

rr_missing() {
  echo "This tool needs the railroad diagram JAR from bottlecaps"
  echo "Download it from https://www.bottlecaps.de/rr/download/rr-1.63-java8.zip"
  echo "put it in your home directory under rr-1.63-java8"
  echo "Or if you have it into a custom folder use -r flag to provide that path. e.g: ./update_docs.sh -r ~/rr-1.63-java8"
  exit 1
}

echo "making clean text version of grammars"

while getopts r: flag
do
    case "${flag}" in
        r) RR=${OPTARG};;
    esac
done

cd ${GRAMMAR_DOCS}

./make_grammar.sh;
./make_json_grammar.sh

echo "copying CQL grammar into CQL Guide Appendix 2"
cp cql_grammar.md "${CQL_GUIDE}/x2.md"

echo "copying JSON grammar into CQL Guide Appendix 5"
cp json_grammar.md "${CQL_GUIDE}/x5.md"

echo "creating Guide HTML files"
cd "${CQL_GUIDE}/generated"
./make_guide.sh

echo "creating Internals HTML files"
./make_internal.sh

echo "making railroad diagrams"

cd ${RR} || rr_missing

cd ${DIAGRAMS}

echo "...CQL grammar"
sed -f "${GRAMMAR_DOCS}/diagram_tweaks.txt" < "${GRAMMAR_DOCS}/cql_grammar.txt" > "${CQL_SOURCES}/out/cql_rr.txt"
java -jar ${RR}/rr.war -out:cql.xhtml "${CQL_SOURCES}/out/cql_rr.txt"

echo "...JSON grammar"
java -jar ${RR}/rr.war -out:json.xhtml "${GRAMMAR_DOCS}/json_grammar.txt"

echo "fixing headers and rewriting xhtml to html in CQL diagram"

cat >cql.html <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><html xmlns="http://www.w3.org/1999/xhtml">
<!--
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
--
-- @generated
-->
EOF

echo "fixing headers and rewriting xhtml to html in JSON diagram"

sed -f rr-replacements.txt <cql.xhtml >>cql.html

cat >json.html <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><html xmlns="http://www.w3.org/1999/xhtml">
<!--
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
--
-- @generated
-->
EOF
sed -f rr-replacements.txt <json.xhtml >>json.html

echo "placing output in the pickup directory"

mv cql.html "${DIAGRAMS}/railroad_diagram.html"
mv json.html "${DIAGRAMS}/json_output_railroad_diagram.html"

echo "cleaning up"

rm cql.xhtml
rm json.xhtml

echo "running diagram font changes"

cd "${DIAGRAMS}"

./format_diagrams.sh

echo "done"
