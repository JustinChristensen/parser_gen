#ifndef AUTO_AST_H_
#define AUTO_AST_H_ 1

#include "parser.h"

#define EXPR_MAX 5000

enum expr_type {
    NULL_EXPR,
    EMPTY_EXPR,
    DOTALL_EXPR,
    ALT_EXPR,
    CAT_EXPR,
    STAR_EXPR,
    PLUS_EXPR,
    OPTIONAL_EXPR,
    SUB_EXPR,
    SYMBOL_EXPR
};

struct expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct expr *lexpr; struct expr *rexpr; };
        // star, plus, optional, sub
        struct { struct expr *expr; };
        // sym
        struct { char symbol; };
        // empty, dotall
    };
};

struct expr_context {
    struct expr *exprbuf;
    struct expr *expr;
    bool has_error;
    struct parse_error error;
};

// expr constructors
struct expr alt_expr(struct expr *lexpr, struct expr *rexpr);
struct expr cat_expr(struct expr *lexpr, struct expr *rexpr);
struct expr star_expr(struct expr *expr);
struct expr plus_expr(struct expr *expr);
struct expr optional_expr(struct expr *expr);
struct expr sub_expr(struct expr *expr);
struct expr symbol_expr(char symbol);
struct expr dotall_expr();
struct expr empty_expr();

// expr context
struct expr_context expr_context(struct expr *exprbuf);
void sexpr(struct expr_context *context, struct expr expr);
struct expr *gexpr(struct expr_context *context);

// parse actions
void do_empty_expr(struct expr_context *context, union rval _);
void do_alt_expr(struct expr_context *context, union rval lexpr);
void do_cat_expr(struct expr_context *context, union rval lexpr);
void do_sub_expr(struct expr_context *context, union rval _);
void do_dotall_expr(struct expr_context *context, union rval _);
void do_symbol_expr(struct expr_context *context, union rval _);
void do_star_expr(struct expr_context *context, union rval _);
void do_plus_expr(struct expr_context *context, union rval _);
void do_optional_expr(struct expr_context *context, union rval _);

// parse action table
void (*const expr_actions[NUMACTIONS])(struct expr_context *context, union rval lval);

#endif // AUTO_AST_H_

