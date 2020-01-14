#ifndef REGEX_DFA_H_
#define REGEX_DFA_H_ 1

#include <stdlib.h>
#include <stdio.h>

struct dfa_loc {
    int line;
    int col;
};

struct dfa_context {
};

struct dfa_match {
    struct dfa_context *context;
    char *input;
    struct dfa_loc input_loc;
    char *match_start;
    struct dfa_loc match_loc;
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
#define RE_REGEX(sym)        { sym, "regex",        "\/[^\/]\/"   }
#define RE_EOF(sym)          { sym, "eof",          ""              }

struct dfa_context dfa_context(struct regex_pattern const **patterns);
void dfa_regex(int sym, char *tag, char *pattern, struct dfa_context *context);
struct dfa_match dfa_match_state(char *input, struct dfa_loc loc, struct dfa_context *context);
int dfa_match(struct dfa_match *match);
struct dfa_loc dfa_match_loc(struct dfa_match *match);
char *dfa_match_lexeme(struct dfa_match *match);
void dfa_print_loc(FILE *handle, struct dfa_loc loc);

#endif // REGEX_DFA_H_
