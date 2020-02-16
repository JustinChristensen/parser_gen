#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "regex/ast.h"
#include "regex/parser.h"
#include "regex/result_types.h"

bool (*expr_actions[])(union regex_result val, void *context) = {
    [AI(DO_REGEX)] =        ACTION noop_expr,
    [AI(DO_EMPTY)] =        ACTION do_empty_expr,
    [AI(DO_ALT)] =          ACTION do_alt_expr,
    [AI(DO_CAT)] =          ACTION do_cat_expr,
    [AI(DO_SUB)] =          ACTION do_sub_expr,
    [AI(DO_TAG)] =          ACTION do_tag_expr,
    [AI(DO_CHAR_CLASS)] =   ACTION do_char_class,
    [AI(DO_NEG_CLASS)] =    ACTION do_neg_class,
    [AI(DO_DOTALL)] =       ACTION do_dotall_expr,
    [AI(DO_SYMBOL)] =       ACTION do_symbol_expr,
    [AI(DO_RANGES)] =       ACTION do_range,
    [AI(DO_RANGE)] =        ACTION do_range,
    [AI(DO_STAR)] =         ACTION do_star_expr,
    [AI(DO_PLUS)] =         ACTION do_plus_expr,
    [AI(DO_OPTIONAL)] =     ACTION do_optional_expr,
    [AI(DO_REPEAT_EXACT)] = ACTION do_repeat_exact_expr
};

// TODO: stop overusing these macros
struct parse_interface expr_pinterface = {
    .result = RESULTFN expr_to_result,
    .has_error = HASERRFN NULL,
    .error = ERRFN NULL
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

struct expr range_expr(struct expr *next, struct char_range range) {
    return (struct expr) {
        .type = RANGE_EXPR,
        .expr = next,
        .range = range
    };
}

struct expr char_class_expr(struct expr *ranges) {
    return (struct expr) {
        .type = CHAR_CLASS_EXPR,
        .ranges = ranges
    };
}

struct expr tag_expr(char *tag) {
    return (struct expr) {
        .type = TAG_EXPR,
        .tag = tag
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

union regex_result expr_to_result(struct expr_context *context) {
    return (union regex_result) { .expr = gexpr(context) };
}

void free_expr_context(struct expr_context *context) {
    struct expr *expr = context->bufstart;

    while (expr != context->bufp) {
        switch (expr->type) {
            case TAG_EXPR:
                free(expr->tag);
                expr->tag = NULL;
                break;
            default:
                break;
        }

        expr++;
    }
}

bool noop_expr(union regex_result _, struct expr_context *context) { return true; }

bool do_empty_expr(union regex_result _, struct expr_context *context) {
    sexpr(context, empty_expr());
    return true;
}

bool do_alt_expr(union regex_result expr, struct expr_context *context) {
    sexpr(context, alt_expr(expr.expr, gexpr(context)));
    return true;
}

bool do_cat_expr(union regex_result expr, struct expr_context *context) {
    sexpr(context, cat_expr(expr.expr, gexpr(context)));
    return true;
}

bool do_sub_expr(union regex_result _, struct expr_context *context) {
    sexpr(context, sub_expr(gexpr(context)));
    return true;
}

bool do_tag_expr(union regex_result tag, struct expr_context *context) {
    char *duptag = strdup(tag.tag);

    if (duptag != NULL) {
        sexpr(context, tag_expr(duptag));
        return true;
    }

    context->has_error = true;
    context->error = oom_error();

    return false;
}

bool do_char_class(union regex_result expr, struct expr_context *context) {
    sexpr(context, char_class_expr(expr.expr));
    return true;
}

bool do_neg_class(union regex_result expr, struct expr_context *context) {
    gexpr(context)->type = NEG_CLASS_EXPR;
    return true;
}

bool do_dotall_expr(union regex_result _, struct expr_context *context) {
    sexpr(context, dotall_expr());
    return true;
}

bool do_symbol_expr(union regex_result sym, struct expr_context *context) {
    sexpr(context, symbol_expr(sym.tval.sym));
    return true;
}

bool do_star_expr(union regex_result _, struct expr_context *context) {
    sexpr(context, star_expr(gexpr(context)));
    return true;
}

bool do_plus_expr(union regex_result _, struct expr_context *context) {
    sexpr(context, plus_expr(gexpr(context)));
    return true;
}

bool do_optional_expr(union regex_result _, struct expr_context *context) {
    sexpr(context, optional_expr(gexpr(context)));
    return true;
}

bool do_repeat_exact_expr(union regex_result num, struct expr_context *context) {
    sexpr(context, repeat_exact_expr(num.tval.num, gexpr(context)));
    return true;
}

bool do_range(union regex_result range, struct expr_context *context) {
    struct expr *prev_range = gexpr(context);
    sexpr(context, range_expr(NULL, range.tval.range));
    prev_range->expr = gexpr(context);
    return true;
}

