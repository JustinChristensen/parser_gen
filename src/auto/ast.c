#include "ast.h"

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

struct expr empty_expr() {
    return (struct expr) {
        .type = EMPTY_EXPR
    };
}

