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
 * zero or one: ?
 * one or more: +
 * character classes: [a-zA-Z]
 */

#define OPERATOR_OFFSET (-256)

enum token_type {
    SYMBOL = -1,
    INVERT = ('^' + OPERATOR_OFFSET),
    ALT = ('|' + OPERATOR_OFFSET),
    STAR = ('*' + OPERATOR_OFFSET),
    PLUS = ('+' + OPERATOR_OFFSET),
    OPTIONAL = ('?' + OPERATOR_OFFSET),
    DOTALL = ('.' + OPERATOR_OFFSET),
    LPAREN = ('(' + OPERATOR_OFFSET),
    RPAREN = (')' + OPERATOR_OFFSET),
    LBRACKET = ('[' + OPERATOR_OFFSET),
    RBRACKET = (']' + OPERATOR_OFFSET)
};

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

struct parse_context {
    struct scan_context scan_context;
    void *result_context;
    int lookahead;
    int lookahead_col;
    bool has_error;
    struct parse_error error;
};

struct scan_context scan_context(char *input);
struct scan_context scan(struct scan_context context);
int token(struct scan_context context);
int token_col(struct scan_context context);
struct parse_context parse_context(char *input, void *result_context);
bool peek(struct parse_context *context, int expected, int (*is) (int c));
bool expect(struct parse_context *context, int expected, int (*is) (int c));
int is_symbol(int c);
int lookahead(struct parse_context *context);
bool has_parse_error(struct parse_context *context);
struct parse_error parse_error(struct parse_context *context);
struct parse_error nullperr();
char *lexeme_for(char *symbuf, int token);
void print_parse_error(struct parse_error error);

#endif // AUTO_PARSER_H_
