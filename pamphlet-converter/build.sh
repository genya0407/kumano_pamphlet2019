#!/bin/bash
set -eu
cat pamphlet2019.tex | stack run output
mkdir -p output/images
#ls .. | grep pdf | grep -v pamphlet | xargs -I{} convert ../{} output/images/{}.png
