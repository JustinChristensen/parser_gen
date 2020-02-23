CFLAGS := -Wall -O0 -g

.PHONY: all
all: libfoo.so libbar.so test

libfoo.so: foo.c
	$(CC) -shared $(CFLAGS) -o $@ $+ 

libbar.so: bar.c
	$(CC) -shared $(CFLAGS) -o $@ $+ 

test: main.c 
	$(CC) $(CFLAGS) -o $@ $+ -L. -lfoo -lbar

.PHONY: clean
clean:
	rm -rf *.so test *.dSYM



