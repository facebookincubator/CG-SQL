#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

rm -f guide.md
(echo -n "<!--- @" ; echo "generated -->") >guide.tmp
for f in ../ch*.md
do
  ( cat "$f"; echo ""; echo "" ) >>guide.tmp
done

for f in ../x*.md
do
  (cat "$f"; \
   echo ""; \
   echo ""; \
   echo "" ) >>guide.tmp
done

# remove the docusaurus coding and replace it with simple markdown for the composite guide.
sed -e '/^id:/d' -e '/^sidebar_label:/d' -e 's/^title: "\(.*\)"/## \1/' -e '/^---$/d' <guide.tmp >guide.md
rm guide.tmp

pandoc --toc -s -f markdown -t html --metadata title="The CQL Programming Language" guide.md -o guide.html
