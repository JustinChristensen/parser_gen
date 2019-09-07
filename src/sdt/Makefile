PROG := sdt
CFLAGS := -Wall -g -O0
LIBCOMMON_DIR := ../../common
LDFLAGS := -L$(LIBCOMMON_DIR) -lcommon
VALGRIND := valgrind

.PHONY: all
all: $(PROG)

$(PROG): main.c ast.c scanner.c parser.c
	$(CC) -o $@ -I../../include $(CFLAGS) $+ $(LDFLAGS)

.PHONY: run
run: export DYLD_LIBRARY_PATH := $(LIBCOMMON_DIR)
run: export LD_LIBRARY_PATH := $(LIBCOMMON_DIR)
run: $(PROG)
	./$(PROG) ./program.txt

.PHONY: check-leaks
check-leaks: export LD_LIBRARY_PATH := $(LIBCOMMON_DIR)
check-leaks: $(PROG)
	$(VALGRIND) --leak-check=full ./$(PROG) ./program.txt

.PHONY: clean
clean:
	rm -rf *.o $(PROG) *.dSYM
