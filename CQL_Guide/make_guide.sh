#!/bin/bash
rm -f guide.md
for f in ch*.md
do
  cat $f >>guide.md
  echo "" >>guide.md
  echo "" >>guide.md
done
  
for f in x*.md
do
  cat $f >>guide.md
  echo "" >>guide.md
  echo '<div style="page-break-after: always; visibility: hidden"></div>' >>guide.md
  echo "" >>guide.md
done
pandoc -f markdown -t html guide.md -o guide.html
