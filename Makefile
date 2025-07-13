BUILD_DIR := build

.PHONY: build http copy-statics clean

build: copy-statics
	mkdir -p $(BUILD_DIR)

	cabal build
	cp -v $(shell dirname $$(cabal list-bin MinesweeperHS-exe))/MinesweeperHS-exe.jsexe/all.js $(BUILD_DIR)/index.js

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
	cabal clean
	rm -rf $(BUILD_DIR)

	@echo ""
	@echo "CLEAN SUCCESSFUL."
	@echo ""
