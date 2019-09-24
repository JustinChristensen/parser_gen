#include "ast.h"

struct expr alt_expr(struct expr lexpr, struct expr rexpr) {
    return (struct expr) {
        .type = ALT,
        .lexpr = lexpr,
        .rexpr = rexpr
    };
}

struct expr cat_expr(struct expr lexpr, struct expr rexpr) {
    return (struct expr) {
        .type = CAT,
        .lexpr = lexpr,
        .rexpr = rexpr
    };
}

struct expr star_expr(struct expr expr) {
    return (struct expr) {
        .type = STAR,
        .expr = expr
    };
}

struct expr sub_expr(struct expr expr) {
    return (struct expr) {
        .type = SUBEXPR,
        .expr = expr
    };
}

struct expr symbol_expr(char symbol) {
    return (struct expr) {
        .type = SYMBOL,
        .symbol = symbol
    };
}

struct expr empty_expr() {
    return (struct expr) {
        .type = EMPTY
    };
}

