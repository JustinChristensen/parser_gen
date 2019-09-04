LIB := libcommon.so
CFLAGS := -Wall -g -O0
LDFLAGS :=

.PHONY: all
all: $(LIB)

$(LIB): linked_list.c
	$(CC) -shared $(CFLAGS) -o $@ $+ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf *.o $(LIB) *.dSYM
