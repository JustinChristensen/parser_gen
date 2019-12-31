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

#define SYMS (enum symbol_type[])
static enum symbol_type *rule_table[] = {
    [ERROR_P] =                SYMS { 0 },
    [EMPTY_P] =                SYMS { 0 },
    [REGEX_P] =                SYMS { EOI, EXPR_NT, 0 },
    [EXPR_ALT_P] =             SYMS { ALT_NT, 0 },
    [ALT_CAT_P] =              SYMS { ALT_TAIL_NT, CAT_NT, 0 },
    [ALT_TAIL_CAT_P] =         SYMS { ALT_TAIL_NT, EXPR_NT, ALT, 0 },
    [CAT_FACTOR_P] =           SYMS { CAT_TAIL_NT, FACTOR_NT, 0 },
    [CAT_TAIL_FACTOR_P] =      SYMS { CAT_TAIL_NT, FACTOR_NT, 0 },
    [FACTOR_SUBEXPR_P] =       SYMS { FACTOR_TAIL_NT, RPAREN, EXPR_NT, LPAREN, 0 },
    [FACTOR_DOTALL_P] =        SYMS { FACTOR_TAIL_NT, DOTALL, 0 },
    [FACTOR_SYMBOL_P] =        SYMS { FACTOR_TAIL_NT, SYMBOL, 0 },
    [FACTOR_TAIL_STAR_P] =     SYMS { FACTOR_TAIL_NT, STAR, 0 },
    [FACTOR_TAIL_PLUS_P] =     SYMS { FACTOR_TAIL_NT, PLUS, 0 },
    [FACTOR_TAIL_OPTIONAL_P] = SYMS { FACTOR_TAIL_NT, OPTIONAL, 0 }
};

void push_sym(int sym, struct array *stack) {
    apush(&sym, stack);
}

void push_production_symbols(enum gram_production production, struct array *stack) {
    enum symbol_type *sym = rule_table[production];
    while (*sym) push_sym(*sym, stack), sym++;
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

            pdebug("parse_table[%s][%s] = %d\n", lexeme_for(sym), lexeme_for(la), p);

            if (p) {
                apop(&sym, stack);
                push_production_symbols(p, stack);
            } else {
                // TODO: make error handling support first/follow sets
                set_parse_error(EOI, context);
                success = false;
            }
        }
    };

    free_array(stack);

    return success;
}

