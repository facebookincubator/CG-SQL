#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

rm -f guide.md
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
pandoc -f markdown -t html guide.md -o guide.html
