TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := libregex.so
LIBS += -lm

include $(TOPDIR)/make/common_targets.mk

$(PROG): ast.c nfa.c parser.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(PROG)" ~/lib/
