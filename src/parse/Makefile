TOPDIR := ../..
include ../../make/*.mk

PROG := parse

.PHONY: all
all: $(PROG)

$(PROG): main.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
