LIB := libbase.so
INCLUDEPATHS := -I/usr/local/include/graphviz -I/usr/include/graphviz
CFLAGS := -Wall -g -O0
LIBS := -lcgraph

.PHONY: all
all: $(LIB)

$(LIB): args.c either.c graphviz.c list.c random.c string.c tuple.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBS) -fpic $(LDFLAGS)
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(LIB)" ~/lib/

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(LIB) *.dSYM
