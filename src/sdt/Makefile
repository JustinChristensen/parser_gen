PROG := sdt
CFLAGS := -Wall -g -O0
LDFLAGS :=

.PHONY: all
all: $(PROG)

$(PROG): main.c scanner.c parser.c
	$(CC) $(CFLAGS) -o $@ $+ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM