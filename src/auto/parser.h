#ifndef AUTO_PARSER_H_
#define AUTO_PARSER_H_ 1

#include <stdbool.h>
#include "ast.h"

/**
 * In order of descending precedence:
 * alternation
 * concatenation
 * kleene
 *
 * Regular Expression Grammar:
 *
 * expr: expr '|' expr
 *     | expr expr
 *     | expr '*'
 *     | '(' expr ')'
 *     | a
 *     | e
 *
 * Without Left Recursion:
 *
 *  A: Aa
 *   | Ab
 *   | c
 *  -------------
 *  A: cR
 *  R: aR
 *   | bR
 *   | e
 *
 * expr: '(' expr ')' expr_rest
 *     | a expr_rest
 *     | e expr_rest
 *
 * expr_rest: '|' expr expr_rest
 *         | expr expr_rest
 *         | '*' expr_rest
 *         | e
 *
 * TODO: extensions
 * zero or one: ?
 * one or more: +
 * character classes: [a-zA-Z]
 */

enum token_type {
    ALT = 256,
    STAR,
    LPAREN,
    RPAREN,
    PRINTABLE
}

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
    char lookahead;
    int lookahead_col;
    bool has_error;
    struct expr expr;
    struct parse_error error;
};

struct scan_context scan_context(char *input);
struct scan_context scan(struct scan_context context);
int token(struct scan_context context);
int token_col(struct scan_context context);
struct parse_context parse_context(char *input);
int peek(struct parse_context *context, int expected, bool (*is) (int c));
int expect(struct parse_context *context, int expected, bool (*is) (int c));
bool is_tail_terminal(struct parse_context *context);
bool parse_expr(struct parse_context *context);
void sexpr(struct parse_context *context, struct expr expr);
struct expr gexpr(struct parse_context *context);
bool has_error(struct parse_context *context);
struct parse_error gerror(struct parse_context *context);
char *lexeme_for(char *symbuf, int token);
void print_error(struct parse_error error);

#endif // AUTO_PARSER_H_
