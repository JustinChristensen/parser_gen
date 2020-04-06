TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := parser_gen
LIBS += -lregex -lgram

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

# heap profiling
# valgrind --tool=massif --stacks=yes --massif-out-file=massif.out ./parser_gen parse example_spec_file.pspec && ms_print massif.out
