OS := $(shell uname)
LIB_DIR := $(TOPDIR)/libs
LIBBASE_DIR := $(LIB_DIR)/base
LIBREGEX_DIR := $(LIB_DIR)/regex
GRAPHVIZ_INCLUDES := -I/usr/local/include/graphviz -I/usr/include/graphviz
override CFLAGS := $(CFLAGS) -Wall -g -O0
LDFLAGS +=
INCLUDEPATHS += -I$(LIBBASE_DIR) -I$(LIBREGEX_DIR) $(GRAPHVIZ_INCLUDES)
LIBPATHS += -L$(LIBBASE_DIR) -L$(LIBREGEX_DIR)
LIBS += -lbase
VALGRIND := valgrind
DOT := dot

