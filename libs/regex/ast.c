#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <base/string.h>
#include "regex/ast.h"
#include "parser.h"

static struct regex_expr alt_expr(struct regex_expr *lexpr, struct regex_expr *rexpr) {
    return (struct regex_expr) {
        .type = RX_ALT_EXPR,
        .lexpr = lexpr,
        .rexpr = rexpr
    };
}

static struct regex_expr cat_expr(struct regex_expr *lexpr, struct regex_expr *rexpr) {
    return (struct regex_expr) {
        .type = RX_CAT_EXPR,
        .lexpr = lexpr,
        .rexpr = rexpr
    };
}

static struct regex_expr star_expr(struct regex_expr *expr) {
    return (struct regex_expr) {
        .type = RX_STAR_EXPR,
        .expr = expr
    };
}

static struct regex_expr plus_expr(struct regex_expr *expr) {
    return (struct regex_expr) {
        .type = RX_PLUS_EXPR,
        .expr = expr
    };
}

static struct regex_expr optional_expr(struct regex_expr *expr) {
    return (struct regex_expr) {
        .type = RX_OPTIONAL_EXPR,
        .expr = expr
    };
}

static struct regex_expr repeat_exact_expr(int num, struct regex_expr *expr) {
    return (struct regex_expr) {
        .type = RX_REPEAT_EXACT_EXPR,
        .expr = expr,
        .num = num
    };
}

static struct regex_expr sub_expr(struct regex_expr *expr) {
    return (struct regex_expr) {
        .type = RX_SUB_EXPR,
        .expr = expr
    };
}

static struct regex_expr range_expr(struct regex_expr *next, struct regex_char_range range) {
    return (struct regex_expr) {
        .type = RX_RANGE_EXPR,
        .expr = next,
        .range = range
    };
}

static struct regex_expr char_class_expr(struct regex_expr *ranges) {
    return (struct regex_expr) {
        .type = RX_CHAR_CLASS_EXPR,
        .ranges = ranges
    };
}

static struct regex_expr tag_expr(char *tag) {
    return (struct regex_expr) {
        .type = RX_TAG_EXPR,
        .tag = tag
    };
}

static struct regex_expr char_expr(char ch) {
    return (struct regex_expr) {
        .type = RX_CHAR_EXPR,
        .ch = ch
    };
}

static struct regex_expr dotall_expr() {
    return (struct regex_expr) {
        .type = RX_DOTALL_EXPR
    };
}

static struct regex_expr empty_expr() {
    return (struct regex_expr) {
        .type = RX_EMPTY_EXPR
    };
}

static void sexpr(struct regex_expr_context *context, struct regex_expr expr) {
    *context->bufp = expr;
    context->expr = context->bufp;
    context->bufp++;
}

static union regex_result expr_to_result(struct regex_expr_context *context) {
    return (union regex_result) { .expr = gexpr(context) };
}

static bool noop_expr(union regex_result _, struct regex_expr_context *context) { return true; }

static bool do_empty_expr(union regex_result _, struct regex_expr_context *context) {
    sexpr(context, empty_expr());
    return true;
}

static bool do_alt_expr(union regex_result expr, struct regex_expr_context *context) {
    sexpr(context, alt_expr(expr.expr, gexpr(context)));
    return true;
}

static bool do_cat_expr(union regex_result expr, struct regex_expr_context *context) {
    sexpr(context, cat_expr(expr.expr, gexpr(context)));
    return true;
}

static bool do_sub_expr(union regex_result _, struct regex_expr_context *context) {
    sexpr(context, sub_expr(gexpr(context)));
    return true;
}

static bool do_tag_expr(union regex_result tag, struct regex_expr_context *context) {
    char *duptag = safedup(tag.tag);

    if (duptag != NULL) {
        sexpr(context, tag_expr(duptag));
        return true;
    }

    context->has_error = true;
    context->error = regex_oom_error();

    return false;
}

static bool do_char_class(union regex_result expr, struct regex_expr_context *context) {
    sexpr(context, char_class_expr(expr.expr));
    return true;
}

static bool do_neg_class(union regex_result expr, struct regex_expr_context *context) {
    gexpr(context)->type = RX_NEG_CLASS_EXPR;
    return true;
}

static bool do_dotall_expr(union regex_result _, struct regex_expr_context *context) {
    sexpr(context, dotall_expr());
    return true;
}

static bool do_char_expr(union regex_result ch, struct regex_expr_context *context) {
    sexpr(context, char_expr(ch.tval.ch));
    return true;
}

static bool do_star_expr(union regex_result _, struct regex_expr_context *context) {
    sexpr(context, star_expr(gexpr(context)));
    return true;
}

static bool do_plus_expr(union regex_result _, struct regex_expr_context *context) {
    sexpr(context, plus_expr(gexpr(context)));
    return true;
}

static bool do_optional_expr(union regex_result _, struct regex_expr_context *context) {
    sexpr(context, optional_expr(gexpr(context)));
    return true;
}

static bool do_repeat_exact_expr(union regex_result num, struct regex_expr_context *context) {
    sexpr(context, repeat_exact_expr(num.tval.num, gexpr(context)));
    return true;
}

static bool do_range(union regex_result range, struct regex_expr_context *context) {
    struct regex_expr *prev_range = gexpr(context);
    sexpr(context, range_expr(NULL, range.tval.range));
    prev_range->expr = gexpr(context);
    return true;
}

static bool (*const expr_actions[])(union regex_result val, struct regex_expr_context *context) = {
    [AI(RX_DO_REGEX)] =        noop_expr,
    [AI(RX_DO_EMPTY)] =        do_empty_expr,
    [AI(RX_DO_ALT)] =          do_alt_expr,
    [AI(RX_DO_CAT)] =          do_cat_expr,
    [AI(RX_DO_SUB)] =          do_sub_expr,
    [AI(RX_DO_TAG)] =          do_tag_expr,
    [AI(RX_DO_CHAR_CLASS)] =   do_char_class,
    [AI(RX_DO_NEG_CLASS)] =    do_neg_class,
    [AI(RX_DO_DOTALL)] =       do_dotall_expr,
    [AI(RX_DO_CHAR)] =         do_char_expr,
    [AI(RX_DO_RANGES)] =       do_range,
    [AI(RX_DO_RANGE)] =        do_range,
    [AI(RX_DO_STAR)] =         do_star_expr,
    [AI(RX_DO_PLUS)] =         do_plus_expr,
    [AI(RX_DO_OPTIONAL)] =     do_optional_expr,
    [AI(RX_DO_REPEAT_EXACT)] = do_repeat_exact_expr
};

struct regex_parse_interface const expr_parse_iface = {
    .result = RESULTFN expr_to_result,
    .has_error = HASERRFN NULL,
    .error = ERRFN NULL,
    .actions = ACTIONS expr_actions
};

struct regex_expr_context expr_context(struct regex_expr *exprbuf) {
    return (struct regex_expr_context) {
        .bufstart = exprbuf,
        .bufp = exprbuf,
        .expr = NULL
    };
}

struct regex_expr *gexpr(struct regex_expr_context *context) {
    return context->expr;
}

void free_expr_context(struct regex_expr_context *context) {
    struct regex_expr *expr = context->bufstart;

    while (expr != context->bufp) {
        switch (expr->type) {
            case RX_TAG_EXPR:
                free(expr->tag);
                expr->tag = NULL;
                break;
            default:
                break;
        }

        expr++;
    }
}

