LIB := libbase.so
INCLUDEPATHS := -I/usr/local/include/graphviz -I/usr/include/graphviz
CFLAGS := -Wall -g -O0
LIBS := -lcgraph -lm

.PHONY: all
all: $(LIB)

$(LIB): array.c args.c either.c graphviz.c list.c random.c stack.c string.c tuple.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBS) -fpic $(LDFLAGS)
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(LIB)" ~/lib/

test: $(LIB) main.c
	$(CC) -I . $(CFLAGS) -o $@ main.c -L . $(LIBS) -lbase $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf test *.o $(LIB) *.dSYM
