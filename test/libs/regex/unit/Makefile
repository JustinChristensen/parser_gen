TOPDIR := ../../../..
include $(TOPDIR)/make/common_vars.mk

GDB := gdb
VALGRIND := valgrind
PROG := test_regex
LIBS += -lcheck -lm

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c nfa_test.c
	$(CC) $(INCLUDEPATHS) $(CFLAGS) -o $@ $+ $(LIBPATHS) $(LIBS) $(LDFLAGS)

.PHONY: check
check: $(PROG)
	./$(PROG)

.PHONY: memcheck
memcheck:
	$(VALGRIND) --leak-check=full ./$(PROG)

.PHONY: debug
debug:
	CK_FORK=no $(GDB) ./$(PROG)
