#!/bin/bash
set -eu
cat pamphlet2019.tex | stack run output
mkdir -p output/images
mkdir -p output/static
cp static/all.css output/static
cp static/slide.js output/static
ls slideimages | xargs -I{} convert -resize 800x slideimages/{} output/{}
ls .. | grep pdf | grep -v pamphlet | xargs -I{} convert ../{} output/images/{}.jpg
