#ifndef REGEX_BASE_H_
#define REGEX_BASE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

enum regex_symbol {
    RX_ERROR,

    // terminals
    RX_EOF_T,
    RX_CHAR_T,
    RX_RANGE_T,
    RX_NUM_T,
    RX_TAG_T,
    RX_ALT_T,
    RX_STAR_T,
    RX_PLUS_T,
    RX_OPTIONAL_T,
    RX_DOTALL_T,
    RX_LPAREN_T,
    RX_RPAREN_T,
    RX_CLASS_T,
    RX_NEG_CLASS_T,
    RX_END_CLASS_T,
    RX_TAG_BRACE_T,
    RX_LBRACE_T,
    RX_RBRACE_T,

    // non-terminals
    RX_REGEX_NT,
    RX_EXPR_NT,
    RX_ALT_NT,
    RX_ALTS_NT,
    RX_FACTOR_NT,
    RX_FACTORS_NT,
    RX_CHAR_CLASS_NT,
    RX_RANGES_NT,
    RX_UNOPS_NT,

    // actions
    RX_DO_REGEX,
    RX_DO_EMPTY,
    RX_DO_ALT,
    RX_DO_CAT,
    RX_DO_SUB,
    RX_DO_TAG,
    RX_DO_CHAR_CLASS,
    RX_DO_NEG_CLASS,
    RX_DO_DOTALL,
    RX_DO_CHAR,
    RX_DO_RANGES,
    RX_DO_RANGE,
    RX_DO_STAR,
    RX_DO_PLUS,
    RX_DO_OPTIONAL,
    RX_DO_REPEAT_EXACT
};

enum regex_error_type {
    RX_SYNTAX_ERROR,
    RX_OUT_OF_MEMORY,
    RX_REPEAT_ZERO,
    RX_TAG_EXISTS,
    RX_MISSING_TAG,
    RX_DUPLICATE_PATTERN
};

struct regex_error {
    enum regex_error_type type;
    union {
        struct {
            int lexeme_col;
            enum regex_symbol actual;
            enum regex_symbol const *expected;
        };
        char *tag;
        char *pattern;
    };
};

struct regex_char_range {
    char start;
    char end;
};

enum expr_type {
    RX_NULL_EXPR,
    RX_EMPTY_EXPR,
    RX_DOTALL_EXPR,
    RX_ALT_EXPR,
    RX_CAT_EXPR,
    RX_SUB_EXPR,
    RX_TAG_EXPR,
    RX_RANGE_EXPR,
    RX_CHAR_CLASS_EXPR,
    RX_NEG_CLASS_EXPR,
    RX_CHAR_EXPR,
    RX_STAR_EXPR,
    RX_PLUS_EXPR,
    RX_OPTIONAL_EXPR,
    RX_REPEAT_EXACT_EXPR
};

struct regex_expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct regex_expr *lexpr; struct regex_expr *rexpr; };
        // star, plus, optional, sub, repeat_exact, range
        struct {
            struct regex_expr *expr;
            union {
                int num;
                struct regex_char_range range;
            };
        };
        // char_class, neg_class
        struct regex_expr *ranges;
        // char
        char ch;
        // tag
        char *tag;
        // empty, dotall
    };
};

enum nfa_state_type {
    RX_ACCEPTING_STATE,
    RX_EPSILON_STATE,
    RX_BRANCH_STATE,
    RX_CHAR_STATE,
    RX_CLASS_STATE,
    RX_DOTALL_STATE
};

struct nfa_state {
    enum nfa_state_type type;
    int id;
    union {
        // epsilon, dotall, char, class
        struct {
            struct nfa_state *next;
            union {
                char ch;
                bool *char_class;
            };
        };
        // branch
        struct { struct nfa_state *left; struct nfa_state *right; };
        // accepting
        struct { int sym; };
    };
};

struct nfa {
    struct nfa_state *start;
    struct nfa_state **end;
    struct nfa_state **end1;
};

union regex_token_val {
    char ch;
    int num;
    struct regex_char_range range;
};

union regex_result {
    char *tag;
    union regex_token_val tval;
    struct regex_expr *expr;
    struct nfa mach;
};

static union regex_result const RX_NULL_RESULT = { 0 };

struct regex_loc {
    int line;
    int col;
};

struct regex_pattern {
    int sym;
    char *tag;
    char *pattern;
};

#define RX_PATTERNS (struct regex_pattern[])
#define RX_END_PATTERNS { 0, NULL, NULL }

enum {
    RX_TAG_ONLY = -2,
    RX_SKIP = -1,
    RX_REJECTED = 0,
    RX_EOF = 1,
    RX_START = 2
};

#define RX_ALPHA(sym)        { sym, "alpha",        "[A-Za-z]"              }
#define RX_ALPHA_(sym)       { sym, "alpha_",       "[A-Za-z_]"             }
#define RX_ALNUM(sym)        { sym, "alnum",        "[0-9A-Za-z]"           }
#define RX_ALNUM_(sym)       { sym, "alnum_",       "[0-9A-Za-z_]"          }
#define RX_SPACE(sym)        { sym, "space",        "[\t\n\v\f\r ]"         }
#define RX_LINE_COMMENT(sym) { sym, "line_comment", "#[^\n]*\n"             }
#define RX_REGEX(sym)        { sym, "regex",        "/(\\\\.|[^\n/])*/"     }

struct regex_error regex_syntax_error(enum regex_symbol const actual, int lexeme_col, enum regex_symbol const *expected);
struct regex_error regex_oom_error();
struct regex_error regex_repeat_zero_error();
struct regex_error regex_missing_tag_error(char *tag);
struct regex_error regex_tag_exists_error(char *tag);
struct regex_error regex_duplicate_pattern_error(char *pattern);
struct regex_error regex_nullerror();
struct regex_loc regex_loc(int line, int col);
struct regex_pattern regex_pattern(int sym, char *tag, char *pattern);
bool regex_null_pattern(struct regex_pattern const *pattern);
struct regex_loc bump_regex_loc(char c, struct regex_loc loc);
char const *str_for_regex_sym(enum regex_symbol type);
void print_regex_error(FILE *handle, struct regex_error error);
void print_regex_loc(FILE *handle, struct regex_loc loc);
void print_regex_range(FILE *handle, struct regex_char_range r);

#endif // REGEX_BASE_H_
