#!/bin/bash
cat pamphlet2019.md | pandoc -f markdown -t html5 --mathjax -s > pamphlet2019.html
