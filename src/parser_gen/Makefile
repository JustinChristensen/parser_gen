TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := parser_gen
LIBS += -lregex -lgram

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)
