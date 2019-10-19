TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := parse

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)
