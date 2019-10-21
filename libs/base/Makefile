TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := libbase.so
LIBS := -lcgraph -lm

include $(TOPDIR)/make/common_targets.mk

$(PROG): array.c args.c bits.c graphviz.c intset.c list.c ord.c random.c string.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(PROG)" ~/lib/

test: $(PROG) main.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ main.c $(LIBPATHS) -lbase $(LDFLAGS)
