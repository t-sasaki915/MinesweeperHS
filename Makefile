BUILD_DIR := build

SHELL := /bin/bash

.PHONY: build debug clean init-tools

build:
	mkdir -p $(BUILD_DIR)

	cp -v -r static/* $(BUILD_DIR)

	cabal v2-build

	@INPUT_JS=$(shell cabal list-bin MinesweeperHS-exe).jsexe/all.js && \
	OUTPUT_JS="$(BUILD_DIR)/index.js" && \
	if [ "$(PRODUCTION)" = "1" ]; then \
		node-minify --compressor uglify-js --input "$$INPUT_JS" --output "$$OUTPUT_JS"; \
	else \
		cp -v "$$INPUT_JS" "$$OUTPUT_JS"; \
	fi

	@echo ""
	@echo "BUILD SUCCESSFUL."
	@echo ""

debug: build
ifeq (, $(shell which http-server))
	sudo npm install -g http-server
endif

	http-server $(BUILD_DIR)

clean:
	cabal v2-clean
	rm -rf $(BUILD_DIR)

	@echo ""
	@echo "CLEAN SUCCESSFUL."
	@echo ""

init-tools:
ifeq (, $(shell which npm))
	$(error Please install npm)
endif
ifeq (, $(shell which curl))
	$(error Please install curl)
endif
	
	make -f <(curl -fsSL https://raw.githubusercontent.com/t-sasaki915/ghc-makefiles/refs/heads/main/javascript-unknown-ghcjs-ghc/Makefile)

	sudo npm install -g @node-minify/cli @node-minify/uglify-js

	@echo ""
	@echo "INIT-ENV SUCCESSFUL."
	@echo ""
