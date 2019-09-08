PROG := sdt
VALGRIND := valgrind
LIBCOMMON_DIR := ../../common
INCLUDEPATHS := -I../../include -I/usr/local/include/graphviz -I/usr/include/graphviz
LIBPATHS := -L$(LIBCOMMON_DIR)
LIBS := -lcommon -lcgraph
CFLAGS := -Wall -g -O0
LDFLAGS :=

.PHONY: all
all: $(PROG)

$(PROG): main.c ast.c dot.c parser.c scanner.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: export DYLD_LIBRARY_PATH := $(LIBCOMMON_DIR)
run: export LD_LIBRARY_PATH := $(LIBCOMMON_DIR)
run: $(PROG)
	./$(PROG) ./program.txt

.PHONY: check-leaks
check-leaks: export LD_LIBRARY_PATH := $(LIBCOMMON_DIR)
check-leaks: $(PROG)
	$(VALGRIND) --leak-check=full ./$(PROG) ./program.txt

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
