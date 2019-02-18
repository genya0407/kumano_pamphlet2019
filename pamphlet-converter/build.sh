#!/bin/bash
set -eu
cat pamphlet2019.tex | stack run output
mkdir -p output/images
mkdir -p output/static
cp static/all.css output/static
convert -bordercolor white -border 100 static/kumano-top.jpg output/kumano-top.jpg
ls .. | grep pdf | grep -v pamphlet | xargs -I{} convert ../{} output/images/{}.jpg
