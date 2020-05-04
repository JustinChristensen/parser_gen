TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := libgram.so
LIBS += -lm -lregex -lcgraph

include $(TOPDIR)/make/common_targets.mk

$(PROG): analyze.c check.c ll.c lr.c parser.c pack.c spec.c states.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(PROG)" ~/lib/
