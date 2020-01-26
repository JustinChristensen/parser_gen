#include <stdlib.h>
#include <stdio.h>
#include "regex/ast.h"
#include "regex/parser.h"
#include "regex/result_types.h"

void (*expr_actions[])(void *context, union rval lval) = {
    [AI(DO_REGEX)] =    ACTION noop_expr,
    [AI(DO_EMPTY)] =    ACTION do_empty_expr,
    [AI(DO_ALT)] =      ACTION do_alt_expr,
    [AI(DO_CAT)] =      ACTION do_cat_expr,
    [AI(DO_SUB)] =      ACTION do_sub_expr,
    [AI(DO_DOTALL)] =   ACTION do_dotall_expr,
    [AI(DO_SYMBOL)] =   ACTION do_symbol_expr,
    [AI(DO_STAR)] =     ACTION do_star_expr,
    [AI(DO_PLUS)] =     ACTION do_plus_expr,
    [AI(DO_OPTIONAL)] = ACTION do_optional_expr
};

struct expr alt_expr(struct expr *lexpr, struct expr *rexpr) {
    return (struct expr) {
        .type = ALT_EXPR,
        .lexpr = lexpr,
        .rexpr = rexpr
    };
}

struct expr cat_expr(struct expr *lexpr, struct expr *rexpr) {
    return (struct expr) {
        .type = CAT_EXPR,
        .lexpr = lexpr,
        .rexpr = rexpr
    };
}

struct expr star_expr(struct expr *expr) {
    return (struct expr) {
        .type = STAR_EXPR,
        .expr = expr
    };
}

struct expr plus_expr(struct expr *expr) {
    return (struct expr) {
        .type = PLUS_EXPR,
        .expr = expr
    };
}

struct expr optional_expr(struct expr *expr) {
    return (struct expr) {
        .type = OPTIONAL_EXPR,
        .expr = expr
    };
}

struct expr sub_expr(struct expr *expr) {
    return (struct expr) {
        .type = SUB_EXPR,
        .expr = expr
    };
}

struct expr symbol_expr(char symbol) {
    return (struct expr) {
        .type = SYMBOL_EXPR,
        .symbol = symbol
    };
}

struct expr dotall_expr() {
    return (struct expr) {
        .type = DOTALL_EXPR
    };
}

struct expr empty_expr() {
    return (struct expr) {
        .type = EMPTY_EXPR
    };
}

struct expr_context expr_context(struct expr *exprbuf) {
    return (struct expr_context) {
        .exprbuf = exprbuf,
        .expr = NULL
    };
}

void sexpr(struct expr_context *context, struct expr expr) {
    *context->exprbuf = expr;
    context->expr = context->exprbuf;
    context->exprbuf++;
}

struct expr *gexpr(struct expr_context *context) {
    return context->expr;
}

union rval expr_to_rval(struct expr_context *context) {
    return (union rval) { .expr = gexpr(context) };
}

void noop_expr(struct expr_context *context, union rval _) {}

void do_empty_expr(struct expr_context *context, union rval _) {
    sexpr(context, empty_expr());
}

void do_alt_expr(struct expr_context *context, union rval lexpr) {
    sexpr(context, alt_expr(lexpr.expr, gexpr(context)));
}

void do_cat_expr(struct expr_context *context, union rval lexpr) {
    sexpr(context, cat_expr(lexpr.expr, gexpr(context)));
}

void do_sub_expr(struct expr_context *context, union rval _) {
    sexpr(context, sub_expr(gexpr(context)));
}

void do_dotall_expr(struct expr_context *context, union rval _) {
    sexpr(context, dotall_expr());
}

void do_symbol_expr(struct expr_context *context, union rval sym) {
    sexpr(context, symbol_expr(sym.sym));
}

void do_star_expr(struct expr_context *context, union rval _) {
    sexpr(context, star_expr(gexpr(context)));
}

void do_plus_expr(struct expr_context *context, union rval _) {
    sexpr(context, plus_expr(gexpr(context)));
}

void do_optional_expr(struct expr_context *context, union rval _) {
    sexpr(context, optional_expr(gexpr(context)));
}

