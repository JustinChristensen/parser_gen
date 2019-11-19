TOPDIR := ../../../..
include $(TOPDIR)/make/common_vars.mk

GDB := gdb
VALGRIND := valgrind
PROG := test_base
LIBS += -lcheck -lm

include $(TOPDIR)/make/common_targets.mk

$(PROG): main.c array_test.c btree_test.c hash_table_test.c intset_test.c
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
