#!/bin/bash

set -e

cabal build --project-file cabal-js.project

mkdir -p build

cp -v $(dirname $(cabal list-bin MinesweeperHS-exe --project-file cabal-js.project))/MinesweeperHS-exe.jsexe/all.js build/index.js

cp -v -r static/* build

echo ""
echo BUILD SUCCESSFUL.
