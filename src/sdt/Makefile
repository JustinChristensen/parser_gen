TOPDIR := ../..
include $(TOPDIR)/make/common_vars.mk

PROG := sdt
LIBS += -lcgraph

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c ast.c dot.c gensource.c parser.c scanner.c source.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: export DYLD_LIBRARY_PATH := $(LIBBASE_DIR)
run: export LD_LIBRARY_PATH := $(LIBBASE_DIR)
run: $(PROG)
	./$(PROG) ./program.txt

program.dot: $(PROG)
	./$(PROG) --format=ast program.txt > program.dot

program.svg: program.dot
	$(DOT) -Tsvg program.dot > program.svg

.PHONY: check-leaks
check-leaks: export LD_LIBRARY_PATH := $(LIBBASE_DIR)
check-leaks: $(PROG)
	$(VALGRIND) --leak-check=full ./$(PROG) ./program.txt
