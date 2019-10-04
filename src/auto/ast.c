#include <stdlib.h>
#include <stdio.h>
#include "ast.h"
#include "parser.h"

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

bool parse_regex(struct parse_context *context) {
    if (parse_expr(context) && expect(context, '\0', NULL)) {
        return true;
    }

    return false;
}

bool parse_expr(struct parse_context *context) {
    parse_alt(context, gexpr(context->result_context));
    return true;
}

bool parse_alt(struct parse_context *context, struct expr *lexpr) {
    struct expr_context *expr_context = context->result_context;

    if (parse_cat(context, lexpr)) {
        while (true) {
            lexpr = gexpr(expr_context);

            if (peek(context, ALT, NULL) &&
                expect(context, ALT, NULL) &&
                parse_expr(context)) {
                sexpr(expr_context, alt_expr(lexpr, gexpr(expr_context)));
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

bool parse_cat(struct parse_context *context, struct expr *lexpr) {
    struct expr_context *expr_context = context->result_context;

    sexpr(expr_context, empty_expr());
    struct expr *empty = gexpr(expr_context);

    if (parse_factor(context)) {
        while (true) {
            lexpr = gexpr(expr_context);

            if (parse_factor(context)) {
                sexpr(expr_context, cat_expr(lexpr, gexpr(expr_context)));
                continue;
            }

            break;
        }

        sexpr(expr_context, cat_expr(empty, gexpr(expr_context)));

        return true;
    }

    return false;
}

bool parse_factor(struct parse_context *context) {
    struct expr_context *expr_context = context->result_context;
    bool has_head = false;

    if (peek(context, LPAREN, NULL) &&
        expect(context, LPAREN, NULL) &&
        parse_expr(context) && expect(context, RPAREN, NULL)) {
        sexpr(expr_context, sub_expr(gexpr(expr_context)));
        has_head = true;
    } else if (peek(context, DOTALL, NULL)) {
        expect(context, DOTALL, NULL);
        sexpr(expr_context, dotall_expr());
        has_head = true;
    } else if (peek(context, SYMBOL, is_symbol)) {
        int sym = lookahead(context);
        expect(context, SYMBOL, is_symbol);
        sexpr(expr_context, symbol_expr((char) sym));
        has_head = true;
    }

    if (has_head) {
        while (true) {
            struct expr *lexpr = gexpr(expr_context);

            if (peek(context, STAR, NULL) && expect(context, STAR, NULL)) {
                sexpr(expr_context, star_expr(lexpr));
                continue;
            } else if (peek(context, PLUS, NULL) && expect(context, PLUS, NULL)) {
                sexpr(expr_context, plus_expr(lexpr));
                continue;
            } else if (peek(context, OPTIONAL, NULL) && expect(context, OPTIONAL, NULL)) {
                sexpr(expr_context, optional_expr(lexpr));
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

void sexpr(struct expr_context *context, struct expr expr) {
    *context->exprbuf = expr;
    context->expr = context->exprbuf;
    context->exprbuf++;
}

struct expr *gexpr(struct expr_context *context) {
    return context->expr;
}

