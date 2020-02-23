OS := $(shell uname)
LIB_DIR := $(TOPDIR)/libs
LIBBASE_DIR := $(LIB_DIR)/base
LIBGRAM_DIR := $(LIB_DIR)/gram
LIBREGEX_DIR := $(LIB_DIR)/regex
GRAPHVIZ_INCLUDES := -I/usr/local/include/graphviz -I/usr/include/graphviz
override CFLAGS := $(CFLAGS) -Wall -g -O0
LDFLAGS +=
INCLUDEPATHS += -I$(LIBBASE_DIR)/include -I$(LIBGRAM_DIR)/include -I$(LIBREGEX_DIR)/include $(GRAPHVIZ_INCLUDES)
LIBPATHS += -L$(LIBBASE_DIR) -L$(LIBGRAM_DIR) -L$(LIBREGEX_DIR)
LIBS += -lbase
VALGRIND := valgrind
DOT := dot

