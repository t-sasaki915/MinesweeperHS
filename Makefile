.PHONY: build copy-statics clean

build: $(SRC_APP)
	mkdir -p build

	cabal build
	cp -v $(shell dirname $$(cabal list-bin MinesweeperHS-exe))/MinesweeperHS-exe.jsexe/all.js build/index.js
	
	$(MAKE) copy-statics

	@echo ""
	@echo "BUILD SUCCESSFUL."

copy-statics: Setup.hs
	mkdir -p build

	cp -v -r static/* build

	@echo ""
	@echo "COPY-STATICS SUCCESSFUL."

clean:
	rm -rf build

	@echo ""
	@echo "CLEAN SUCCESSFUL."
