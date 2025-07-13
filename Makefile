BUILD_DIR := build

.PHONY: build copy-statics clean

build:
	mkdir -p $(BUILD_DIR)

	cabal build
	cp -v $(shell dirname $$(cabal list-bin MinesweeperHS-exe))/MinesweeperHS-exe.jsexe/all.js $(BUILD_DIR)/index.js
	
	$(MAKE) copy-statics

	@echo ""
	@echo "BUILD SUCCESSFUL."

http:
	$(MAKE) build

	http-server $(BUILD_DIR)

copy-statics:
	mkdir -p $(BUILD_DIR)

	cp -v -r static/* $(BUILD_DIR)

	@echo ""
	@echo "COPY-STATICS SUCCESSFUL."

clean:
	cabal clean
	rm -rf $(BUILD_DIR)

	@echo ""
	@echo "CLEAN SUCCESSFUL."
