TOPDIR := ../..
include ../../make/*.mk

LIB := libbase.so
LIBS := -lcgraph -lm

.PHONY: all
all: $(LIB)

$(LIB): array.c args.c graphviz.c intset.c list.c random.c string.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(LIB)" ~/lib/

test: $(LIB) main.c
	$(CC) -I . $(CFLAGS) -o $@ main.c -L . $(LIBS) -lbase $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf test *.o $(LIB) *.dSYM
