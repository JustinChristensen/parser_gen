PROG := auto
LIBBASE_DIR := ../../base
INCLUDEPATHS := -I$(LIBBASE_DIR) -I/usr/local/include/graphviz -I/usr/include/graphviz
LIBPATHS := -L$(LIBBASE_DIR)
LIBS := -lbase -lcgraph
override CFLAGS := -Wall -g -O0 $(CFLAGS)
LDFLAGS :=

.PHONY: all
all: $(PROG)

$(PROG): main.c ast.c dot.c parser.c print_ast.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
