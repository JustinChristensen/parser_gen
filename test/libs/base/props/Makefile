CABAL := cabal
CABAL_BUILD := $(CABAL) v2-build
CABAL_TEST := $(CABAL) v2-test
CABAL_REPL := $(CABAL) v2-repl

.PHONY: all
all: Main.hs
	$(CABAL_BUILD)

.PHONY: check
check: all
	$(CABAL_TEST)

.PHONY: repl
repl: all
	$(CABAL_REPL)

.PHONY: clean
clean:
	rm -rf dist dist-newstyle
