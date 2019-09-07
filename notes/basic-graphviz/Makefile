PROG := graphviz-test
CFLAGS := -Wall -O0
LDFLAGS :=
INCLUDES := -I/usr/local/include/graphviz
LIBPATHS := -L/usr/local/lib
LIBS := -lcgraph
CHROME := open -a Google\ Chrome.app
DOT := dot

.PHONY: all
all: main.c
	$(CC) $(INCLUDES) $(CFLAGS) -o $(PROG) $+ $(LIBPATHS) $(LIBS)

.PHONY: run
run: all
	./$(PROG) | dot -Tsvg > graph.svg && $(CHROME) graph.svg


.PHONY: clean
clean:
	rm -rf $(PROG) *.o *.dSYM
