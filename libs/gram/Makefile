TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := libgram.so
LIBS += -lm -lregex

include $(TOPDIR)/make/common_targets.mk

$(PROG): parser.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(PROG)" ~/lib/
