#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <base/string.h>
#include <regex/ast.h>
#include <regex/parser.h>
#include "print_ast.h"

void print_expr(struct regex_expr *expr) {
    // expression stack
    struct regex_expr **exprs, **sp;
    exprs = sp = calloc(RX_EXPR_MAX, sizeof *exprs);
    assert(exprs != NULL);

    *sp++ = expr;

    int indent_level = 0;
    while (sp > exprs) {
        expr = *--sp;

        if (expr == NULL) {
            indent_level--;
        } else {
            switch (expr->type) {
                case RX_NULL_EXPR:
                    fprintf(stderr, "null expression encountered\n");
                    break;
                case RX_EMPTY_EXPR:
                    indent(stdout, indent_level);
                    printf("ε");
                    break;
                case RX_DOTALL_EXPR:
                    indent(stdout, indent_level);
                    printf(".");
                    break;
                case RX_ALT_EXPR:
                    indent(stdout, indent_level);
                    printf("|");
                    indent_level++;
                    *sp++ = NULL; // use NULL as a sentinel to decrease the indent level
                    *sp++ = expr->rexpr;
                    *sp++ = expr->lexpr;
                    break;
                case RX_CAT_EXPR:
                    indent(stdout, indent_level);
                    printf("+");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->rexpr;
                    *sp++ = expr->lexpr;
                    break;
                case RX_STAR_EXPR:
                    indent(stdout, indent_level);
                    printf("*");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_PLUS_EXPR:
                    indent(stdout, indent_level);
                    printf("+");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_OPTIONAL_EXPR:
                    indent(stdout, indent_level);
                    printf("?");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_REPEAT_EXACT_EXPR:
                    indent(stdout, indent_level);
                    printf("{%d}", expr->num);
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_SUB_EXPR:
                    indent(stdout, indent_level);
                    printf("()");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_CHAR_CLASS_EXPR:
                    indent(stdout, indent_level);
                    printf("[]");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_NEG_CLASS_EXPR:
                    indent(stdout, indent_level);
                    printf("[^]");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_RANGE_EXPR:
                    indent(stdout, indent_level);
                    print_regex_range(stdout, expr->range);
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RX_CHAR_EXPR:
                    indent(stdout, indent_level);
                    printf("%c", expr->ch);
                    break;
                case RX_TAG_EXPR:
                    indent(stdout, indent_level);
                    printf("{%s}", expr->tag);
                    break;
            }

            printf("\n");
        }
    }

    free(exprs);
    exprs = sp = NULL;
}

void print_expr_table(struct regex_expr *start, struct regex_expr *end) {
    while (start != end) {
        printf("%p: (", start);
        switch (start->type) {
            case RX_NULL_EXPR:
                break;
            case RX_EMPTY_EXPR:
                printf("ε");
                break;
            case RX_DOTALL_EXPR:
                printf(".");
                break;
            case RX_ALT_EXPR:
                printf("| %p %p", start->lexpr, start->rexpr);
                break;
            case RX_CAT_EXPR:
                printf("+ %p %p", start->lexpr, start->rexpr);
                break;
            case RX_STAR_EXPR:
                printf("* %p", start->expr);
                break;
            case RX_PLUS_EXPR:
                printf("+ %p", start->expr);
                break;
            case RX_OPTIONAL_EXPR:
                printf("? %p", start->expr);
                break;
            case RX_REPEAT_EXACT_EXPR:
                printf("{%d} %p", start->num, start->expr);
                break;
            case RX_SUB_EXPR:
                printf("() %p", start->expr);
                break;
            case RX_CHAR_CLASS_EXPR:
                printf("[] %p", start->expr);
                break;
            case RX_NEG_CLASS_EXPR:
                printf("[^] %p", start->expr);
                break;
            case RX_RANGE_EXPR:
                print_regex_range(stdout, start->range);
                printf(" %p", start->expr);
                break;
            case RX_TAG_EXPR:
                printf("{%s}", start->tag);
                break;
            case RX_CHAR_EXPR:
                printf("%c", start->ch);
                break;
        }

        printf(")\n");
        start++;
    }
}
