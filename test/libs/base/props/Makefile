CABAL := cabal
CABAL_BUILD := $(CABAL) v2-build
CABAL_TEST := $(CABAL) v2-test

.PHONY: all
all: Main.hs
	$(CABAL_BUILD) --enable-tests

.PHONY: check
check: all
	$(CABAL_TEST)

.PHONY: clean
clean:
	rm -rf dist dist-newstyle
