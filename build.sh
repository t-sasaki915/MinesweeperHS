#!/bin/bash

set -e

wasm32-wasi-cabal update --project-file cabal-wasm.project
wasm32-wasi-cabal build --allow-newer --project-file cabal-wasm.project

mkdir -p build

$(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
    --input=$(wasm32-wasi-cabal list-bin MinesweeperHS-exe --allow-newer) \
    --output=build/ghc_wasm_jsffi.js

cp -v $(wasm32-wasi-cabal list-bin MinesweeperHS-exe --allow-newer) build

cp -v static/* build
