TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := libregex.so
LIBS += -lm

include $(TOPDIR)/make/common_targets.mk

$(PROG): ast.c base.c nfa.c parser.c parser_rec.c run_parser.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(PROG)" ~/lib/
