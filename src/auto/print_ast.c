#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <base/string.h>
#include <regex/ast.h>
#include <regex/parser.h>
#include "print_ast.h"

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
                case DOTALL_EXPR:
                    indent(indent_level);
                    printf(".");
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
                case PLUS_EXPR:
                    indent(indent_level);
                    printf("+");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case OPTIONAL_EXPR:
                    indent(indent_level);
                    printf("?");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case REPEAT_EXACT_EXPR:
                    indent(indent_level);
                    printf("{%d}", expr->num);
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
                case CHAR_CLASS_EXPR:
                    indent(indent_level);
                    printf("[]");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case NEG_CLASS_EXPR:
                    indent(indent_level);
                    printf("[^]");
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case RANGE_EXPR:
                    indent(indent_level);
                    regex_print_range(stdout, expr->range);
                    indent_level++;
                    *sp++ = NULL;
                    *sp++ = expr->expr;
                    break;
                case CHAR_EXPR:
                    indent(indent_level);
                    printf("%c", expr->ch);
                    break;
                case TAG_EXPR:
                    indent(indent_level);
                    printf("{%s}", expr->tag);
                    break;
            }

            printf("\n");
        }
    }

    free(exprs);
    exprs = sp = NULL;
}

void print_expr_table(struct expr *start, struct expr *end) {
    while (start != end) {
        printf("%p: (", start);
        switch (start->type) {
            case NULL_EXPR:
                break;
            case EMPTY_EXPR:
                printf("ε");
                break;
            case DOTALL_EXPR:
                printf(".");
                break;
            case ALT_EXPR:
                printf("| %p %p", start->lexpr, start->rexpr);
                break;
            case CAT_EXPR:
                printf("+ %p %p", start->lexpr, start->rexpr);
                break;
            case STAR_EXPR:
                printf("* %p", start->expr);
                break;
            case PLUS_EXPR:
                printf("+ %p", start->expr);
                break;
            case OPTIONAL_EXPR:
                printf("? %p", start->expr);
                break;
            case REPEAT_EXACT_EXPR:
                printf("{%d} %p", start->num, start->expr);
                break;
            case SUB_EXPR:
                printf("() %p", start->expr);
                break;
            case CHAR_CLASS_EXPR:
                printf("[] %p", start->expr);
                break;
            case NEG_CLASS_EXPR:
                printf("[^] %p", start->expr);
                break;
            case RANGE_EXPR:
                regex_print_range(stdout, start->range);
                printf(" %p", start->expr);
                break;
            case TAG_EXPR:
                printf("{%s}", start->tag);
                break;
            case CHAR_EXPR:
                printf("%c", start->ch);
                break;
        }

        printf(")\n");
        start++;
    }
}
