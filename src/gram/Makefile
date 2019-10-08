PROG := gram
LIBBASE_DIR := ../../base
INCLUDEPATHS := -I$(LIBBASE_DIR)
LIBPATHS := -L$(LIBBASE_DIR)
LIBS := -lbase
override CFLAGS := -Wall -g -O0 $(CFLAGS)
LDFLAGS :=

.PHONY: all
all: $(PROG)

$(PROG): main.c parser.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
