PROG := graphviz-test
CFLAGS := -Wall -O0
LDFLAGS :=
INCLUDEPATHS := -I/usr/local/include/graphviz
LIBPATHS :=
LIBS := -lcgraph
CHROME := open -a Google\ Chrome.app
DOT := dot

.PHONY: all
all: main.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $(PROG) $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: all
	./$(PROG) | dot -Tsvg > graph.svg && $(CHROME) graph.svg


.PHONY: clean
clean:
	rm -rf $(PROG) *.o *.dSYM
