TOPDIR := ../../..
include $(TOPDIR)/make/common_vars.mk

GDB := gdb
PROG := test_base
LIBS += -lcheck

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c intset_test.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: check
check: $(PROG)
	./$(PROG)

.PHONY: props
props:
	$(MAKE) -C props check

.PHONY: debug
debug:
	CK_FORK=no $(GDB) ./$(PROG)
