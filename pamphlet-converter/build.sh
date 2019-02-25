#!/bin/bash
set -eu
cat pamphlet2019.tex | stack run output
mkdir -p output/images
mkdir -p output/static
cp static/all.css output/static
cp static/slide.js output/static
#convert slideimages/slide*.JPG -resize 1193x846 resized_slideimages/resized_slide_%02d.jpg
#convert -delay 200 -loop 0 slideimages/resized_slide_*.jpg static/kumano-top.gif
cp slideimages/* output/
#convert -bordercolor white -border 100x0 static/kumano-top.jpg output/kumano-top.jpg
ls .. | grep pdf | grep -v pamphlet | xargs -I{} convert ../{} output/images/{}.jpg
