#ifndef REGEX_BASE_H_
#define REGEX_BASE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct regex_char_range {
    char start;
    char end;
};

struct regex_loc {
    int line;
    int col;
};

struct regex_pattern {
    int sym;
    char *tag;
    char *pattern;
};

static struct regex_pattern const RX_NULL_PATTERN = { 0, NULL, NULL };

#define RX_PATTERNS (struct regex_pattern[])
#define RX_END_PATTERNS RX_NULL_PATTERN

enum {
    RX_TAG_ONLY = -3,
    RX_REJECTED = -2,
    RX_EOF = -1
};

#define RX_ALPHA(sym)        { sym, "alpha",        "[A-Za-z]"      }
#define RX_ALPHA_(sym)       { sym, "alpha_",       "[A-Za-z_]"     }
#define RX_ALNUM(sym)        { sym, "alnum",        "[0-9A-Za-z]"   }
#define RX_ALNUM_(sym)       { sym, "alnum_",       "[0-9A-Za-z_]"  }
#define RX_SPACE(sym)        { sym, "space",        "[\t\n\v\f\r ]" }
#define RX_LINE_COMMENT(sym) { sym, "line_comment", "//[^\n]*\n"    }
#define RX_REGEX(sym)        { sym, "regex",        "/[^/\n]/"      }

struct regex_loc regex_loc(int line, int col);
struct regex_loc bump_regex_loc(char c, struct regex_loc loc);
void print_regex_loc(FILE *handle, struct regex_loc loc);
void print_regex_range(FILE *handle, struct regex_char_range r);

#endif // REGEX_BASE_H_
