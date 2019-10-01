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
    sexpr(context->result_context, empty_expr());
    parse_alt(context, gexpr(context->result_context));
    return true;
}

bool parse_alt(struct parse_context *context, struct expr *lexpr) {
    if (parse_cat(context, lexpr)) {
        while (true) {
            lexpr = gexpr(context->result_context);

            if (peek(context, ALT, NULL) &&
                expect(context, ALT, NULL) &&
                parse_expr(context)) {
                sexpr(context->result_context, alt_expr(lexpr, gexpr(context->result_context)));
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

bool parse_cat(struct parse_context *context, struct expr *lexpr) {
    if (parse_factor(context)) {
        while (true) {
            lexpr = gexpr(context->result_context);

            if (parse_factor(context)) {
                sexpr(context->result_context, cat_expr(lexpr, gexpr(context->result_context)));
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

bool parse_factor(struct parse_context *context) {
    bool has_head = false;

    if (peek(context, LPAREN, NULL) &&
        expect(context, LPAREN, NULL) &&
        parse_expr(context) && expect(context, RPAREN, NULL)) {
        sexpr(context->result_context, sub_expr(gexpr(context->result_context)));
        has_head = true;
    } else if (peek(context, SYMBOL, is_symbol)) {
        int sym = lookahead(context);
        expect(context, SYMBOL, is_symbol);
        sexpr(context->result_context, symbol_expr((char) sym));
        has_head = true;
    }

    if (has_head) {
        while (true) {
            if (peek(context, STAR, NULL) && expect(context, STAR, NULL)) {
                sexpr(context->result_context, star_expr(gexpr(context->result_context)));
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

