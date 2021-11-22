#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

(echo -n "<!--- @" ; echo "generated -->") >internal.tmp
for f in ../int*.md
do
  ( cat "$f"; echo ""; echo "" ) >>internal.tmp
done

# remove the docusaurus coding and replace it with simple markdown for the composite guide.
sed -e '/^id:/d' -e '/^sidebar_label:/d' -e 's/^title: "\(.*\)"/## \1/' -e '/^---$/d' <internal.tmp >internal.md
rm internal.tmp

pandoc --toc -s -f markdown -t html --metadata title="CQL Internals" internal.md -o internal.html
