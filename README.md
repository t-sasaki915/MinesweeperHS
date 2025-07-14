# MinesweeperHS
Minesweeper written with Haskell &amp; ghcjs

# Usage
## Install tools
These commands install `javascript-unknown-ghcjs-ghc` and `node-minify` to your system.
If you have it, you can skip this step.
```bash
sudo apt-get install npm curl git

# Please see https://www.haskell.org/ghcup/install/#system-requirements
sudo apt-get install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 pkg-config

make init-tools # You may be asked for your password because it runs 'sudo npm install -g'.
source ~/.bashrc
```
## Build
```bash
cabal v2-update
make # or 'make build'
```
## Debug
```bash
make debug
```
