LIB := libbase.so
CFLAGS := -Wall -g -O0
LDFLAGS := -fpic

.PHONY: all
all: $(LIB)

$(LIB): graphviz.c args.c list.c random.c string.c tuple.c
	$(CC) -shared $(CFLAGS) -o $@ $+ $(LDFLAGS)
	mkdir -p ~/lib
	ln -sf "$$(pwd)/$(LIB)" ~/lib/

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(LIB) *.dSYM
