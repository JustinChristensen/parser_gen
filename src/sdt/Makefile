PROG := sdt
VALGRIND := valgrind
LIBBASE_DIR := ../../base
INCLUDEPATHS := -I$(LIBBASE_DIR) -I/usr/local/include/graphviz -I/usr/include/graphviz
LIBPATHS := -L$(LIBBASE_DIR)
LIBS := -lbase -lcgraph
CFLAGS := -Wall -g -O0
LDFLAGS :=

.PHONY: all
all: $(PROG)

$(PROG): main.c ast.c dot.c parser.c scanner.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: export DYLD_LIBRARY_PATH := $(LIBBASE_DIR)
run: export LD_LIBRARY_PATH := $(LIBBASE_DIR)
run: $(PROG)
	./$(PROG) ./program.txt

.PHONY: check-leaks
check-leaks: export LD_LIBRARY_PATH := $(LIBBASE_DIR)
check-leaks: $(PROG)
	$(VALGRIND) --leak-check=full ./$(PROG) ./program.txt

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
