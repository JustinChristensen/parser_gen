PROG := stacktest

.PHONY: all
all: $(PROG)

$(PROG): main.c
	$(CC) -Wall -o $@ $+

clean:
	rm -rf *.o $(PROG)
