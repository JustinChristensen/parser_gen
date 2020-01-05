TARGETS := all clean
SUBDIRS := \
libs \
src/auto \
src/parser_gen \
src/sdt \
test

include make/subdirs.mk

.PHONY: check
check:
	$(MAKE) -C ./test check

.PHONY: props
props:
	$(MAKE) -C test props
