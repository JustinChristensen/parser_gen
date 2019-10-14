TOPDIR := ../..
include ../../make/*.mk

LIB := libregex.so
LIBS += -lm

.PHONY: all
all: $(LIB)

$(LIB): ast.c nfa.c parser.c
	$(CC) $(INCLUDEPATHS) -shared $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS) -fpic
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(LIB)" ~/lib/

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(LIB) *.dSYM
