#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "parser.h"
#include "print_ast.h"
#include "ast.h"

static void indent(int n) {
    while (n-- > 0) printf("    ");
}

void print_expr(struct expr *expr) {
    // expression stack
    struct expr **exprs, **sp;
    exprs = sp = calloc(EXPR_MAX, sizeof *exprs);
    assert(exprs != NULL);

    *sp++ = expr;

    int indent_level = 0;
    while (sp > exprs) {
        expr = *--sp;

        if (expr == NULL) {
            indent_level--;
        } else {
            switch (expr->type) {
                case NULL_EXPR:
                    fprintf(stderr, "null expression encountered\n");
                    break;
                case EMPTY_EXPR:
                    indent(indent_level);
                    printf("ε");
                    break;
                case ALT_EXPR:
                    indent(indent_level);
                    printf("|");
                    indent_level++;
                    *sp++ = NULL; // use NULL as a sentinel to decrease the indent level
                    *sp++ = expr->rexpr;
                    *sp++ = expr->lexpr;
                    break;
                case CAT_EXPR:
                    indent(indent_level);
                    printf("+");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->rexpr;
                    *sp++ = expr->lexpr;
                    break;
                case STAR_EXPR:
                    indent(indent_level);
                    printf("*");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case SUB_EXPR:
                    indent(indent_level);
                    printf("()");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case SYMBOL_EXPR:
                    indent(indent_level);
                    printf("%c", expr->symbol);
                    break;
            }

            printf("\n");
        }
    }

    free(exprs);
    exprs = sp = NULL;
}
