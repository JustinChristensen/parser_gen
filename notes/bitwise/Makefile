PROG := bits

.PHONY: all
all: $(PROG)

$(PROG): main.c
	$(CC) -g -O0 -Wall -o $@ $+

.PHONY: clean
clean:
	rm -rf $(PROG) *.dSYM



