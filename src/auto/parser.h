#ifndef AUTO_PARSER_H_
#define AUTO_PARSER_H_ 1

#include <stdbool.h>

/**
 * Grammar:
 *
 * regex: expr eof
 * expr: alt
 * alt: cat alt_tail
 * alt_tail: + cat { alt } alt_tail | ε
 * cat: ε { empty } factor cat_tail { cat }
 * cat_tail: factor { cat } cat_tail | ε
 * factor: ( expr ) { sub } factor_tail
 *       | [ insymbolseq ] { class } factor_tail
 *       | . { dotall } factor_tail
 *       | a { sym } factor_tail
 * factor_tail: * { star } factor_tail
 *            | + { plus } factor_tail
 *            | ? { optional } factor_tail | ε
 * insymbolseq: ^ symbolseq_tail { invert }
 *            | symbolseq_tail
 * symbolseq_tail: symbol { and } symbolseq_tail | ε
 * symbol: a { sym } | a-z { symrange }
 *
 * TODO: extensions
 * character classes: [a-zA-Z]
 */

#define OPERATOR_OFFSET (-256)

enum token_type {
    SYMBOL = -1,
    ALT = ('|' + OPERATOR_OFFSET),
    STAR = ('*' + OPERATOR_OFFSET),
    PLUS = ('+' + OPERATOR_OFFSET),
    OPTIONAL = ('?' + OPERATOR_OFFSET),
    DOTALL = ('.' + OPERATOR_OFFSET),
    LPAREN = ('(' + OPERATOR_OFFSET),
    RPAREN = (')' + OPERATOR_OFFSET)
};

enum action_type {
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

#define NUMACTIONS DO_OPTIONAL + 1

struct scan_context {
    char *input;
    int input_col;
    int token;
    int token_col;
};

struct parse_error {
    int token_col;
    int expected;
    int actual;
};

union rval {
    struct expr *expr;
    struct nfa_machine mach;
    char sym;
};

struct parse_context {
    void *result_context;
    void (*const actions[NUMACTIONS])(void *result_context, union rval lval);
    union rval (*getval)(void *result_context);
    struct scan_context scan_context;
    int lookahead;
    int lookahead_col;
    bool has_error;
    struct parse_error error;
};

struct scan_context scan_context(char *input);
struct scan_context consume(struct scan_context context, char c);
struct scan_context scan(struct scan_context context);
int token(struct scan_context context);
int token_col(struct scan_context context);
void noopaction(void *result_context, union rval _);
union rval getval(struct parse_context *context);
void do_action(struct parse_context *context, enum action_type action, union rval lval);
struct parse_context parse_context(
    char *input,
    void *result_context,
    union rval (*getval)(void *result_context),
    void (*const *actions)(void *result_context, void *lastval)
);
bool peek(struct parse_context *context, int expected, int (*is) (int c));
bool expect(struct parse_context *context, int expected, int (*is) (int c));
bool parse_regex(struct parse_context *context);
bool parse_expr(struct parse_context *context);
bool parse_alt(struct parse_context *context, union rval lval);
bool parse_cat(struct parse_context *context, union rval lval);
bool parse_factor(struct parse_context *context);
int is_symbol(int c);
int lookahead(struct parse_context *context);
bool has_parse_error(struct parse_context *context);
struct parse_error parse_error(struct parse_context *context);
struct parse_error nullperr();
char *lexeme_for(char *symbuf, int token);
void print_parse_error(struct parse_error error);

#endif // AUTO_PARSER_H_
