PROG := argstest
CFLAGS := -Wall -O0
LDFLAGS :=

.PHONY: all
all: main.c
	$(CC) $(CFLAGS) -o $(PROG) $+ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf $(PROG) *.o *.dSYM
