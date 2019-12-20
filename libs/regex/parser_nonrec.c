#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "regex/parser_shared.h"
#include "regex/result_types.h"
#include "base/array.h"

void push_sym(int sym, struct array *stack) {
    apush(&sum, stack);
}

bool parse_regex_nonrec(struct parse_context *context) {
    enum gram_production **parse_table = context->parse_table;
    struct array *stack = init_array(sizeof(int), PARSE_STACK_SIZE, 0, 0);
    int sym;

    push_sym(REGEX_SYM, stack);

    while (!has_parse_error(context) && !aempty(stack)) {
        apeek(&sym, stack);

        enum gram_production p = parse_table[sym][token];

        switch (p) {
            case REGEX_P:
                // DO_REGEX
                push_sym('\0', stack);
                push_sym(EXPR_NT, stack);
                break;
            case EXPR_ALT_P:
                push_sym(ALT_NT, stack);
                break;
            case ALT_CAT_P:
                push_sym(ALT_TAIL_NT, stack);
                push_sym(CAT_NT, stack);
                break;
            case ALT_TAIL_PLUS_P:
                push_sym(ALT_TAIL_NT, stack);
                // DO_ALT
                push_sym(EXPR_NT, stack);
                push_sym(ALT, stack);
                break;
            case ALT_TAIL_EMPTY_P:
                break;
            case CAT_FACTOR_P:
                // DO_CAT
                push_sym(CAT_TAIL_NT, stack);
                // DO_ALT
                push_sym(FACTOR_NT, stack);
                // DO_EMPTY
                break;
            case CAT_TAIL_FACTOR_P:
                push_sym(CAT_TAIL_NT, stack);
                // DO_CAT
                push_sym(FACTOR_NT, stack);
                break;
            case CAT_TAIL_EMPTY_P:
                break;
            case FACTOR_SUBEXPR_P:
                push_sym(FACTOR_TAIL_NT, stack);
                // DO_SUB
                push_sym(RPAREN, stack);
                push_sym(EXPR_NT, stack);
                push_sym(LPAREN, stack);
                break;
            case FACTOR_DOTALL_P:
                push_sym(FACTOR_TAIL_NT, stack);
                // DO_DOTALL
                push_sym(DOTALL, stack);
                break;
            case FACTOR_SYMBOL_P:
                push_sym(FACTOR_TAIL_NT, stack);
                // DO_SYMBOL
                push_sym(SYMBOL, stack);
                break;
            case FACTOR_TAIL_STAR_P:
                push_sym(FACTOR_TAIL_NT, stack);
                // DO_STAR
                push_sym(STAR, stack);
                break;
            case FACTOR_TAIL_PLUS_P:
                push_sym(FACTOR_TAIL_NT, stack);
                // DO_PLUS
                push_sym(PLUS, stack);
                break;
            case FACTOR_TAIL_OPTIONAL_P:
                push_sym(FACTOR_TAIL_NT, stack);
                // DO_OPTIONAL
                push_sym(OPTIONAL, stack);
                break;
        }
    };

    free_array(stack);
}
