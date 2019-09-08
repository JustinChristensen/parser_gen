LIB := libbase.so
CFLAGS := -Wall -g -O0
LDFLAGS := -fpic

.PHONY: all
all: $(LIB)

$(LIB): graphviz.c linked_list.c string.c
	$(CC) -shared $(CFLAGS) -o $@ $+ $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(LIB) *.dSYM
