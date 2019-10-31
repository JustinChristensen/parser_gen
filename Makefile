TARGETS := all clean
SUBDIRS := \
libs \
src/auto \
src/parse \
src/sdt \
test

include make/subdirs.mk

.PHONY: check
check:
	$(MAKE) -C ./test check

.PHONY: props
props:
	$(MAKE) -C test props
