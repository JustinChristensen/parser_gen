#ifndef AUTO_AST_H_
#define AUTO_AST_H_ 1

#include "parser.h"

#define EXPR_MAX 5000

enum expr_type {
    NULL_EXPR,
    EMPTY_EXPR,
    ALT_EXPR,
    CAT_EXPR,
    STAR_EXPR,
    SUB_EXPR,
    SYMBOL_EXPR
};

struct expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct expr *lexpr; struct expr *rexpr; };
        // star, sub
        struct { struct expr *expr; };
        // sym
        struct { char symbol; };
        // empty
    };
};

struct expr_context {
    struct expr *exprbuf;
    struct expr *expr;
    bool has_error;
    struct parse_error error;
};

struct expr alt_expr(struct expr *lexpr, struct expr *rexpr);
struct expr cat_expr(struct expr *lexpr, struct expr *rexpr);
struct expr star_expr(struct expr *expr);
struct expr sub_expr(struct expr *expr);
struct expr symbol_expr(char symbol);
struct expr empty_expr();

struct expr_context expr_context(struct expr *exprbuf);
bool parse_regex(struct parse_context *context);
bool parse_expr(struct parse_context *context);
bool parse_alt(struct parse_context *context, struct expr *lexpr);
bool parse_cat(struct parse_context *context, struct expr *lexpr);
bool parse_factor(struct parse_context *context);
void sexpr(struct expr_context *context, struct expr expr);
struct expr *gexpr(struct expr_context *context);

#endif // AUTO_AST_H_

