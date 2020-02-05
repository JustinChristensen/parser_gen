#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "regex/ast.h"
#include "regex/parser.h"
#include "regex/result_types.h"

bool (*expr_actions[])(union regex_result val, struct parse_context *context) = {
    [AI(DO_REGEX)] =        noop_expr,
    [AI(DO_EMPTY)] =        do_empty_expr,
    [AI(DO_ALT)] =          do_alt_expr,
    [AI(DO_CAT)] =          do_cat_expr,
    [AI(DO_SUB)] =          do_sub_expr,
    [AI(DO_ID)] =           do_id_expr,
    [AI(DO_CHAR_CLASS)] =   do_char_class,
    [AI(DO_NEG_CLASS)] =    do_neg_class,
    [AI(DO_DOTALL)] =       do_dotall_expr,
    [AI(DO_SYMBOL)] =       do_symbol_expr,
    [AI(DO_RANGE)] =        do_range,
    [AI(DO_STAR)] =         do_star_expr,
    [AI(DO_PLUS)] =         do_plus_expr,
    [AI(DO_OPTIONAL)] =     do_optional_expr,
    [AI(DO_REPEAT_EXACT)] = do_repeat_exact_expr
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

union regex_result expr_to_result(struct expr_context *context) {
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

bool noop_expr(union regex_result _, struct parse_context *context) { return true; }

bool do_empty_expr(union regex_result _, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, empty_expr());
    return true;
}

bool do_alt_expr(union regex_result expr, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, alt_expr(expr.expr, gexpr(rcontext)));
    return true;
}

bool do_cat_expr(union regex_result expr, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, cat_expr(expr.expr, gexpr(rcontext)));
    return true;
}

bool do_sub_expr(union regex_result _, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, sub_expr(gexpr(rcontext)));
    return true;
}

bool do_id_expr(union regex_result id, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    char *dupid = strdup(id.id);

    if (dupid != NULL) {
        sexpr(rcontext, id_expr(dupid));
        return true;
    }

    return set_oom_error(context);
}

bool do_char_class(union regex_result expr, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, char_class_expr(expr.expr));
    return true;
}

bool do_neg_class(union regex_result expr, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    gexpr(rcontext)->type = NEG_CLASS_EXPR;
    return true;
}

bool do_dotall_expr(union regex_result _, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, dotall_expr());
    return true;
}

bool do_symbol_expr(union regex_result sym, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, symbol_expr(sym.tval.sym));
    return true;
}

bool do_star_expr(union regex_result _, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, star_expr(gexpr(rcontext)));
    return true;
}

bool do_plus_expr(union regex_result _, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, plus_expr(gexpr(rcontext)));
    return true;
}

bool do_optional_expr(union regex_result _, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, optional_expr(gexpr(rcontext)));
    return true;
}

bool do_repeat_exact_expr(union regex_result num, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    sexpr(rcontext, repeat_exact_expr(num.tval.num, gexpr(rcontext)));
    return true;
}

bool do_range(union regex_result range, struct parse_context *context) {
    struct expr_context *rcontext = context->result_context;
    struct expr *prev_range = gexpr(rcontext);
    sexpr(rcontext, range_expr(NULL, range.tval.range));
    prev_range->expr = gexpr(rcontext);
    return true;
}

