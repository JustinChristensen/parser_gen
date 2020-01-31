#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "regex/ast.h"
#include "regex/parser.h"
#include "regex/result_types.h"

void (*expr_actions[])(void *context, union regex_result lval) = {
    [AI(DO_REGEX)] =        ACTION noop_expr,
    [AI(DO_EMPTY)] =        ACTION do_empty_expr,
    [AI(DO_ALT)] =          ACTION do_alt_expr,
    [AI(DO_CAT)] =          ACTION do_cat_expr,
    [AI(DO_SUB)] =          ACTION do_sub_expr,
    [AI(DO_ID)] =           ACTION do_id_expr,
    [AI(DO_CHAR_CLASS)] =   ACTION noop_expr,
    [AI(DO_NEG_CLASS)] =    ACTION noop_expr,
    [AI(DO_DOTALL)] =       ACTION do_dotall_expr,
    [AI(DO_SYMBOL)] =       ACTION do_symbol_expr,
    [AI(DO_RANGE)] =        ACTION noop_expr,
    [AI(DO_STAR)] =         ACTION do_star_expr,
    [AI(DO_PLUS)] =         ACTION do_plus_expr,
    [AI(DO_OPTIONAL)] =     ACTION do_optional_expr,
    [AI(DO_REPEAT_EXACT)] = ACTION do_repeat_exact_expr
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

struct expr repeat_exact_expr(int num, struct expr *expr) {
    return (struct expr) {
        .type = REPEAT_EXACT_EXPR,
        .expr = expr,
        .num = num
    };
}

struct expr sub_expr(struct expr *expr) {
    return (struct expr) {
        .type = SUB_EXPR,
        .expr = expr
    };
}

struct expr id_expr(char *id) {
    return (struct expr) {
        .type = ID_EXPR,
        .id = id
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
        .bufstart = exprbuf,
        .bufp = exprbuf,
        .expr = NULL
    };
}

void sexpr(struct expr_context *context, struct expr expr) {
    *context->bufp = expr;
    context->expr = context->bufp;
    context->bufp++;
}

struct expr *gexpr(struct expr_context *context) {
    return context->expr;
}

union regex_result expr_to_rval(struct expr_context *context) {
    return (union regex_result) { .expr = gexpr(context) };
}

void free_expr_context(struct expr_context *context) {
    struct expr *expr = context->bufstart;

    while (expr != context->bufp) {
        switch (expr->type) {
            case ID_EXPR:
                free(expr->id);
                expr->id = NULL;
                break;
            default:
                break;
        }

        expr++;
    }
}

void noop_expr(struct expr_context *context, union regex_result _) {}

void do_empty_expr(struct expr_context *context, union regex_result _) {
    sexpr(context, empty_expr());
}

void do_alt_expr(struct expr_context *context, union regex_result expr) {
    sexpr(context, alt_expr(expr.expr, gexpr(context)));
}

void do_cat_expr(struct expr_context *context, union regex_result expr) {
    sexpr(context, cat_expr(expr.expr, gexpr(context)));
}

void do_sub_expr(struct expr_context *context, union regex_result _) {
    sexpr(context, sub_expr(gexpr(context)));
}

void do_id_expr(struct expr_context *context, union regex_result id) {
    // TODO: semi-monadic either thing on a context?
    char *dupid = strdup(id.id);
    assert(dupid != NULL);
    sexpr(context, id_expr(dupid));
}

void do_dotall_expr(struct expr_context *context, union regex_result _) {
    sexpr(context, dotall_expr());
}

void do_symbol_expr(struct expr_context *context, union regex_result sym) {
    sexpr(context, symbol_expr(sym.tval.sym));
}

void do_star_expr(struct expr_context *context, union regex_result _) {
    sexpr(context, star_expr(gexpr(context)));
}

void do_plus_expr(struct expr_context *context, union regex_result _) {
    sexpr(context, plus_expr(gexpr(context)));
}

void do_optional_expr(struct expr_context *context, union regex_result _) {
    sexpr(context, optional_expr(gexpr(context)));
}

void do_repeat_exact_expr(struct expr_context *context, union regex_result num) {
    sexpr(context, repeat_exact_expr(num.tval.num, gexpr(context)));
}

