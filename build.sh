#!/bin/bash

set -e

cabal build

mkdir -p build

cp -v $(dirname $(cabal list-bin MinesweeperHS-exe))/MinesweeperHS-exe.jsexe/all.js build/index.js

cp -v -r static/* build

echo ""
echo BUILD SUCCESSFUL.
