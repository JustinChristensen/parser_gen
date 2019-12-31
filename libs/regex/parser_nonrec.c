#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "regex/parser_nonrec.h"
#include "regex/parser_shared.h"
#include "regex/result_types.h"
#include "base/array.h"
#include "base/debug.h"

// non-terminal index
#define pdebug(...) debug_ns_("parser_nonrec", __VA_ARGS__);
#define NTI(sym) (sym - NUM_TERMINALS)

static enum gram_production parse_table[NUM_NONTERMINALS][NUM_TERMINALS] = {
    [NTI(REGEX_NT)] = {
        [EOI] = REGEX_P,
        [SYMBOL] = REGEX_P,
        [DOTALL] = REGEX_P,
        [LPAREN] = REGEX_P
    },
    [NTI(EXPR_NT)] = {
        [EOI] = EMPTY_P,
        [SYMBOL] = EXPR_ALT_P,
        [DOTALL] = EXPR_ALT_P,
        [LPAREN] = EXPR_ALT_P,
        [RPAREN] = EMPTY_P
    },
    [NTI(ALT_NT)] = {
        [SYMBOL] = ALT_CAT_P,
        [DOTALL] = ALT_CAT_P,
        [LPAREN] = ALT_CAT_P
    },
    [NTI(ALT_TAIL_NT)] = {
        [EOI] = EMPTY_P,
        [ALT] = ALT_TAIL_CAT_P,
        [RPAREN] = EMPTY_P
    },
    [NTI(CAT_NT)] = {
        [SYMBOL] = CAT_FACTOR_P,
        [DOTALL] = CAT_FACTOR_P,
        [LPAREN] = CAT_FACTOR_P
    },
    [NTI(CAT_TAIL_NT)] = {
        [EOI] = EMPTY_P,
        [SYMBOL] = CAT_TAIL_FACTOR_P,
        [ALT] = EMPTY_P,
        [DOTALL] = CAT_TAIL_FACTOR_P,
        [LPAREN] = CAT_TAIL_FACTOR_P,
        [RPAREN] = EMPTY_P
    },
    [NTI(FACTOR_NT)] = {
        [SYMBOL] = FACTOR_SYMBOL_P,
        [DOTALL] = FACTOR_DOTALL_P,
        [LPAREN] = FACTOR_SUBEXPR_P
    },
    [NTI(FACTOR_TAIL_NT)] = {
        [EOI] = EMPTY_P,
        [SYMBOL] = EMPTY_P,
        [ALT] = EMPTY_P,
        [STAR] = FACTOR_TAIL_STAR_P,
        [PLUS] = FACTOR_TAIL_PLUS_P,
        [OPTIONAL] = FACTOR_TAIL_OPTIONAL_P,
        [DOTALL] = EMPTY_P,
        [LPAREN] = EMPTY_P,
        [RPAREN] = EMPTY_P
    }
};

void push_sym(int sym, struct array *stack) {
    apush(&sym, stack);
}

void push_production_symbols(enum gram_production production, struct array *stack) {
    assert(production != ERROR_P);

    switch (production) {
        case ERROR_P:
        case EMPTY_P: break;
        case REGEX_P:
            // DO_REGEX
            push_sym(EOI, stack);
            push_sym(EXPR_NT, stack);
            break;
        case EXPR_ALT_P:
            push_sym(ALT_NT, stack);
            break;
        case ALT_CAT_P:
            push_sym(ALT_TAIL_NT, stack);
            push_sym(CAT_NT, stack);
            break;
        case ALT_TAIL_CAT_P:
            push_sym(ALT_TAIL_NT, stack);
            // DO_ALT
            push_sym(EXPR_NT, stack);
            push_sym(ALT, stack);
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
}

static enum gram_production selectp(int nonterm, enum symbol_type term) {
    return parse_table[nonterm][term];
}

bool is_terminal(int sym) {
    return sym < REGEX_NT;
}

static void debug_sym_stack(struct array *stack) {
    int sym;

    pdebug("symbol stack: ");
    for (int i = 0; i < asize(stack); i++) {
        at(&sym, i, stack);
        debug_("%s ", lexeme_for(sym));
    }

    debug_("\n");
}

bool parse_regex_nonrec(struct parse_context *context) {
    struct array *stack = init_array(sizeof(enum symbol_type), PARSE_STACK_SIZE, 0, 0);
    bool success = true;
    enum symbol_type sym;

    push_sym(REGEX_NT, stack);

    while (success && !aempty(stack)) {
        apeek(&sym, stack);

        debug_sym_stack(stack);

        if (is_terminal(sym)) {
            if (expect(context, sym))
                apop(&sym, stack);
            else
                success = false;
        } else {
            enum symbol_type la = lookahead(context);
            enum gram_production p = selectp(NTI(sym), la);

            pdebug("[%s][%s] = %d\n", lexeme_for(sym), lexeme_for(la), p);

            if (p) {
                apop(&sym, stack);
                push_production_symbols(p, stack);
            } else {
                set_parse_error(EOI, context);
                success = false;
            }
        }
    };

    free_array(stack);

    return success;
}

