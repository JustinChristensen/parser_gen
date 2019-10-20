TOPDIR := ../../..
include $(TOPDIR)/make/common_vars.mk

PROG := test_base
LIBS += -lcheck

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c intset_test.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

check: $(PROG)
	./$(PROG)

