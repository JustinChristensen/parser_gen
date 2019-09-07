LIB := libcommon.so
CFLAGS := -Wall -g -O0
LDFLAGS := -fpic

.PHONY: all
all: $(LIB)

$(LIB): linked_list.c
	$(CC) -shared -I../include $(CFLAGS) -o $@ $+ $(LDFLAGS)

.PHONY: run
run: ;

.PHONY: clean
clean:
	rm -rf *.o $(LIB) *.dSYM
