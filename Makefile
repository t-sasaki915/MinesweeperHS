BUILD_DIR := build
SHELL     := /bin/bash

.PHONY: build http copy-statics clean init-env

build: copy-statics
	mkdir -p $(BUILD_DIR)

	cabal v2-build

	node-minify --compressor uglify-js --input '$(shell dirname $$(cabal list-bin MinesweeperHS-exe))/MinesweeperHS-exe.jsexe/all.js' --output '$(BUILD_DIR)/index.js'

	@echo ""
	@echo "BUILD SUCCESSFUL."
	@echo ""

http: build
	http-server $(BUILD_DIR)

copy-statics:
	mkdir -p $(BUILD_DIR)

	cp -v -r static/* $(BUILD_DIR)

	@echo ""
	@echo "COPY-STATICS SUCCESSFUL."
	@echo ""

clean:
	cabal v2-clean
	rm -rf $(BUILD_DIR)

	@echo ""
	@echo "CLEAN SUCCESSFUL."
	@echo ""

init-env:
ifeq (, $(shell which npm))
	$(error Please install npm)
endif
ifeq (, $(shell which curl))
	$(error Please install curl)
endif
	
	export BOOTSTRAP_HASKELL_NONINTERACTIVE=1; \
	export BOOTSTRAP_HASKELL_MINIMAL=1; \
	export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1; \
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

	bash -c "source ~/.bashrc && ghcup install cabal 3.14.2.0"

	git clone https://github.com/emscripten-core/emsdk.git ~/.emsdk
	~/.emsdk/emsdk install 3.1.74
	~/.emsdk/emsdk activate 3.1.74
	echo "" >> ~/.bashrc
	echo "source ~/.emsdk/emsdk_env.sh" >> ~/.bashrc
	echo "" >> ~/.bashrc

	bash -c "source ~/.bashrc && ghcup config add-release-channel cross"
	bash -c "source ~/.bashrc && emconfigure ghcup install ghc --set javascript-unknown-ghcjs-9.12.2"

	sudo npm install -g @node-minify/cli @node-minify/uglify-js

	@echo ""
	@echo "INIT-ENV SUCCESSFUL."
	@echo ""
