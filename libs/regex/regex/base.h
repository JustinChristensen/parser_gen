#ifndef REGEX_BASE_H_
#define REGEX_BASE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct char_range {
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

static struct regex_pattern const NULL_PATTERN = { 0, NULL, NULL };

#define RE_PATTERNS (struct regex_pattern[])
#define RE_END_PATTERNS NULL_PATTERN

enum {
    RE_TAG_ONLY = -3,
    RE_REJECTED = -2,
    RE_EOF = -1
};

#define RE_ALPHA(sym)        { sym, "alpha",        "[A-Za-z]"      }
#define RE_ALPHA_(sym)       { sym, "alpha_",       "[A-Za-z_]"     }
#define RE_ALNUM(sym)        { sym, "alnum",        "[0-9A-Za-z]"   }
#define RE_ALNUM_(sym)       { sym, "alnum_",       "[0-9A-Za-z_]"  }
#define RE_SPACE(sym)        { sym, "space",        "[\t\n\v\f\r ]" }
#define RE_LINE_COMMENT(sym) { sym, "line_comment", "//[^\n]*\n"    }
#define RE_REGEX(sym)        { sym, "regex",        "/[^/\n]/"        }

struct regex_loc bump_loc(char c, struct regex_loc loc);
struct regex_loc regex_loc(int line, int col);
void regex_print_loc(FILE *handle, struct regex_loc loc);
void regex_print_range(FILE *handle, struct char_range r);
bool is_null_pattern(struct regex_pattern const *pattern);

#endif // REGEX_BASE_H_
