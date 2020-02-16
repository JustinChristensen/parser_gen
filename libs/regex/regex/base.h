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

#define PATTERNS (struct regex_pattern[])
#define END_PATTERNS NULL_PATTERN

#define RE_ALPHA(sym)        { sym, "alpha",        "[A-Za-z]"      }
#define RE_ALNUM(sym)        { sym, "alnum",        "[0-9A-Za-z]"   }
#define RE_SPACE(sym)        { sym, "space",        "[\t\n\v\f\r ]" }
#define RE_LINE_COMMENT(sym) { sym, "line_comment", "\/\/[^\n]\n"   }
#define RE_REGEX(sym)        { sym, "regex",        "\/[^\/]\/"     }
#define RE_EOF(sym)          { sym, "eof",          ""              }

struct regex_loc regex_loc(int line, int col);
void regex_print_loc(FILE *handle, struct regex_loc loc);
void regex_print_range(FILE *handle, struct char_range r);
bool is_null_pattern(struct regex_pattern const *pattern);

#endif // REGEX_BASE_H_
