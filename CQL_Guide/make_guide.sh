#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

rm -f guide.md
(echo -n "<!--- @" ; echo "generated -->") >guide.md
for f in ch*.md
do
  ( cat "$f"; echo ""; echo "" ) >>guide.md
done
  
for f in x*.md
do
  (cat "$f"; \
   echo ""; \
   echo ""; \
   echo '<div style="page-break-after: always; visibility: hidden"></div>'; \
   echo "" ) >>guide.md
done
pandoc --toc -s -f markdown -t html --metadata title="The CQL Programming Language" guide.md -o guide.html
