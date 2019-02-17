#!/bin/bash
set -eu
./build.sh
cd output; python3 -m http.server 8000; cd ..
