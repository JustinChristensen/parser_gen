PROG := props-test
CABAL := cabal
VALGRIND := valgrind
CABAL_BUILD := $(CABAL) v2-build
CABAL_TEST := $(CABAL) v2-run CABAL_REPL := $(CABAL) v2-repl

.PHONY: all
all: Main.hs
	$(CABAL_BUILD)

.PHONY: check
check:
	$(CABAL_TEST)

.PHONY: repl
repl:
	$(CABAL_REPL)

.PHONY: memcheck
memcheck:
	binpath=$$(find dist-newstyle -type f -executable -name '$(PROG)') && \
	$(VALGRIND) --leak-check=full $$binpath

.PHONY: clean
clean:
	rm -rf dist dist-newstyle
