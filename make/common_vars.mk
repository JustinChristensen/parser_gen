OS := $(shell uname)
ifeq ($(OS),Darwin)
CC := /usr/local/opt/llvm/bin/clang
endif
LIB_DIR := $(TOPDIR)/libs
LIBBASE_DIR := $(LIB_DIR)/base
LIBGRAM_DIR := $(LIB_DIR)/gram
LIBREGEX_DIR := $(LIB_DIR)/regex
INCLUDE_DIR := include
GRAPHVIZ_INCLUDES := -I/usr/local/include/graphviz -I/usr/include/graphviz
DEBUG_FLAGS := -g -O0
ifdef OPTIMIZE
    DEBUG_FLAGS := -O3
endif
override CFLAGS := $(CFLAGS) -Wall -Wextra -Wno-missing-field-initializers $(DEBUG_FLAGS)
# -fsanitize=address
override LDFLAGS := $(LDFLAGS)
INCLUDEPATHS += -I/usr/local/include -I$(LIBBASE_DIR)/$(INCLUDE_DIR) -I$(LIBGRAM_DIR)/$(INCLUDE_DIR) -I$(LIBREGEX_DIR)/$(INCLUDE_DIR) $(GRAPHVIZ_INCLUDES)
LIBPATHS += -L/usr/local/lib -L$(LIBBASE_DIR) -L$(LIBGRAM_DIR) -L$(LIBREGEX_DIR)
LIBS += -lbase
VALGRIND := valgrind
DOT := dot



