#ifndef REGEX_PARSER_H_
#define REGEX_PARSER_H_ 1

#include <stdbool.h>
#include "result_types.h"

/**
 * Grammar:
 *
 * regex: expr eof { regex }
 * expr: alt | ε
 * alt: cat alt_tail
 * alt_tail: + expr { alt } alt_tail | ε
 * cat: ε { empty } factor cat_tail { cat }
 * cat_tail: factor { cat } cat_tail | ε
 * factor: ( expr ) { sub } factor_tail
 *       | . { dotall } factor_tail
 *       | a { sym } factor_tail
 * factor_tail: * { star } factor_tail
 *            | + { plus } factor_tail
 *            | ? { optional } factor_tail
 *            | ε
 *
 * TODO: extensions
 * character classes: [a-zA-Z]
 */

#define GETVALFN (union rval (*) (void *))
#define ACTION (void (*)(void *, union rval))
#define ERROR_FMT_STRING "| Parse Error\n|\n| Got: %s\n| Expected: %s\n|\n| At Column: %d\n|\n"

enum regex_symbol {
    NONSYM,

    // terminals
    EOF_T,
    SYMBOL_T,
    ALT_T,
    STAR_t,
    PLUS_T,
    OPTIONAL_T,
    DOTALL_T,
    LPAREN_T,
    RPAREN_T,
    CLASS_T,
    NEG_CLASS_T,
    END_CLASS_T,
    RANGE_T,
    LBRACE_T,
    INT_T,
    RBRACE_T,

    // non-terminals (used during iterative parsing)
    REGEX_NT,
    EXPR_NT,
    ALT_NT,
    ALT_TAIL_NT,
    CAT_NT,
    CAT_TAIL_NT,
    FACTOR_NT,
    FACTOR_TAIL_NT,

    // parser actions
    DO_REGEX,
    DO_EMPTY,
    DO_ALT,
    DO_CAT,
    DO_SUB,
    DO_DOTALL,
    DO_SYMBOL,
    DO_STAR,
    DO_PLUS,
    DO_OPTIONAL
};

#define NUM_SYMBOLS (DO_OPTIONAL + 1)
#define NUM_TERMINALS (RPAREN + 1)
#define NUM_NONTERMINALS ((FACTOR_TAIL_NT + 1) - NUM_TERMINALS)
#define NUM_ACTIONS ((DO_OPTIONAL + 1) - NUM_NONTERMINALS)
// non-terminal index
#define NTI(sym) (sym - NUM_TERMINALS)
// do action index
#define AI(sym) (sym - NUM_NONTERMINALS)

enum gram_production {
    ERROR_P,

    // expr: ε
    // alt_tail: ε
    // cat_tail: ε
    // factor_tail: ε
    EMPTY_P,

    // regex: expr eof { regex }
    REGEX_P,

    // expr: alt
    EXPR_ALT_P,

    // alt: cat alt_tail
    ALT_CAT_P,

    // alt_tail: + cat { alt } alt_tail
    ALT_TAIL_CAT_P,

    // cat: ε { empty } factor cat_tail
    CAT_FACTOR_P,

    // cat_tail: factor { cat } cat_tail
    CAT_TAIL_FACTOR_P,

    // factor: ( expr ) { sub } factor_tail
    FACTOR_SUBEXPR_P,
    // factor: . { dotall } factor_tail
    FACTOR_DOTALL_P,
    // factor: a { sym } factor_tail
    FACTOR_SYMBOL_P,

    // factor_tail: * { star } factor_tail
    FACTOR_TAIL_STAR_P,
    // factor_tail: + { plus } factor_tail
    FACTOR_TAIL_PLUS_P,
    // factor_tail: ? { optional } factor_tail
    FACTOR_TAIL_OPTIONAL_P
};

struct scan_context {
    char *input;
    int input_col;
    int token;
    char symbol;
    int token_col;
};

struct parse_error {
    int token_col;
    int expected;
    int actual;
};

struct parse_context {
    void *result_context;
    void (**actions)(void *result_context, union rval lval);
    union rval (*getval)(void *result_context);
    struct scan_context scan_context;
    enum regex_symbol lookahead;
    int lookahead_col;
    char symbol;
    bool has_error;
    struct parse_error error;
    bool use_nonrec;
};

struct scan_context scan_context(char *input);
struct scan_context consume(struct scan_context context, char c);
struct scan_context scan(struct scan_context context);
enum regex_symbol token(struct scan_context context);
char token_sym(struct scan_context context);
int token_col(struct scan_context context);
union rval getval(struct parse_context *context);
void do_action(struct parse_context *context, enum regex_symbol action, union rval lval);
struct parse_context parse_context(
    char *input,
    void *result_context,
    union rval (*getval)(void *result_context),
    void (**actions)(void *result_context, union rval lval),
    bool use_nonrec
);
bool peek(struct parse_context *context, enum regex_symbol expected);
bool expect(struct parse_context *context, enum regex_symbol expected);
int is_symbol(int c);
enum regex_symbol lookahead(struct parse_context *context);
char symbol(struct parse_context *context);
void set_parse_error(enum regex_symbol expected, struct parse_context *context);
bool has_parse_error(struct parse_context *context);
struct parse_error parse_error(struct parse_context *context);
struct parse_error nullperr();
char *lexeme_for(enum regex_symbol token);
void print_parse_error(struct parse_error error);

#endif // REGEX_PARSER_H_
