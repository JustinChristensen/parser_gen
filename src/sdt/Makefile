PROG := sdt
CFLAGS := -Wall -g -O0
LDFLAGS := -L../../common -lcommon

.PHONY: all
all: $(PROG)

$(PROG): main.c ast.c scanner.c parser.c
	$(CC) -I../../include $(CFLAGS) -o $@ $+ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
