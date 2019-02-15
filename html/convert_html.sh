#!/bin/bash
#ls .. | grep pdf | grep -v png | grep -v pamphlet2019.pdf | xargs -I{} convert ../{} {}.png
cat ../pamphlet2019.tex | sed -e 's/\.pdf/&.png/g' | pandoc -f latex -t html5 --mathjax -s > pamphlet2019.html
