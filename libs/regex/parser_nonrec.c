#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <base/array.h>
#include <base/debug.h>
#include "regex/parser.h"
#include "regex/parser_nonrec.h"
#include "regex/result_types.h"

#define pdebug(...) debug_ns_("parser_nonrec", __VA_ARGS__);

static enum gram_production parse_table[NUM_NONTERMINALS][NUM_TERMINALS] = {
    [NTI(REGEX_NT)] = {
        [EOF_T] = REGEX_P,
        [SYMBOL_T] = REGEX_P,
        [DOTALL_T] = REGEX_P,
        [LPAREN_T] = REGEX_P
    },
    [NTI(EXPR_NT)] = {
        [EOF_T] = EMPTY_P,
        [SYMBOL_T] = EXPR_ALT_P,
        [DOTALL_T] = EXPR_ALT_P,
        [LPAREN_T] = EXPR_ALT_P,
        [RPAREN_T] = EMPTY_P
    },
    [NTI(ALT_NT)] = {
        [SYMBOL_T] = ALT_CAT_P,
        [DOTALL_T] = ALT_CAT_P,
        [LPAREN_T] = ALT_CAT_P
    },
    [NTI(ALT_TAIL_NT)] = {
        [EOF_T] = EMPTY_P,
        [ALT_T] = ALT_TAIL_CAT_P,
        [RPAREN_T] = EMPTY_P
    },
    [NTI(CAT_NT)] = {
        [SYMBOL_T] = CAT_FACTOR_P,
        [DOTALL_T] = CAT_FACTOR_P,
        [LPAREN_T] = CAT_FACTOR_P
    },
    [NTI(CAT_TAIL_NT)] = {
        [EOF_T] = EMPTY_P,
        [SYMBOL_T] = CAT_TAIL_FACTOR_P,
        [ALT_T] = EMPTY_P,
        [DOTALL_T] = CAT_TAIL_FACTOR_P,
        [LPAREN_T] = CAT_TAIL_FACTOR_P,
        [RPAREN_T] = EMPTY_P
    },
    [NTI(FACTOR_NT)] = {
        [SYMBOL_T] = FACTOR_SYMBOL_P,
        [DOTALL_T] = FACTOR_DOTALL_P,
        [LPAREN_T] = FACTOR_SUBEXPR_P
    },
    [NTI(FACTOR_TAIL_NT)] = {
        [EOF_T] = EMPTY_P,
        [SYMBOL_T] = EMPTY_P,
        [ALT_T] = EMPTY_P,
        [STAR_T] = FACTOR_TAIL_STAR_P,
        [PLUS_T] = FACTOR_TAIL_PLUS_P,
        [OPTIONAL_T] = FACTOR_TAIL_OPTIONAL_P,
        [DOTALL_T] = EMPTY_P,
        [LPAREN_T] = EMPTY_P,
        [RPAREN_T] = EMPTY_P
    }
};

#define SYMS (enum regex_symbol[])
static enum regex_symbol *rule_table[] = {
    [ERROR_P] =                SYMS { 0 },
    [EMPTY_P] =                SYMS { 0 },
    [REGEX_P] =                SYMS { DO_REGEX, EOF_T, EXPR_NT, 0 },
    [EXPR_ALT_P] =             SYMS { ALT_NT, 0 },
    [ALT_CAT_P] =              SYMS { ALT_TAIL_NT, CAT_NT, 0 },
    [ALT_TAIL_CAT_P] =         SYMS { ALT_TAIL_NT, DO_ALT, EXPR_NT, ALT_T, 0 },
    [CAT_FACTOR_P] =           SYMS { DO_CAT, CAT_TAIL_NT, FACTOR_NT, DO_EMPTY, 0 },
    [CAT_TAIL_FACTOR_P] =      SYMS { CAT_TAIL_NT, DO_CAT, FACTOR_NT, 0 },
    [FACTOR_SUBEXPR_P] =       SYMS { FACTOR_TAIL_NT, DO_SUB, RPAREN_T, EXPR_NT, LPAREN_T, 0 },
    [FACTOR_DOTALL_P] =        SYMS { FACTOR_TAIL_NT, DO_DOTALL, DOTALL_T, 0 },
    [FACTOR_SYMBOL_P] =        SYMS { FACTOR_TAIL_NT, DO_SYMBOL, SYMBOL_T, 0 },
    [FACTOR_TAIL_STAR_P] =     SYMS { FACTOR_TAIL_NT, DO_STAR, STAR_T, 0 },
    [FACTOR_TAIL_PLUS_P] =     SYMS { FACTOR_TAIL_NT, DO_PLUS, PLUS_T, 0 },
    [FACTOR_TAIL_OPTIONAL_P] = SYMS { FACTOR_TAIL_NT, DO_OPTIONAL, OPTIONAL_T, 0 }
};

static void push_sym(int sym, struct array *stack) { apush(&sym, stack); }
static void push_rval(union rval lval, struct array *results) { apush(&lval, results); }

static void push_production_symbols(enum gram_production production, struct array *stack) {
    enum regex_symbol *sym = rule_table[production];
    while (*sym) push_sym(*sym, stack), sym++;
}

static enum gram_production selectp(int nonterm, enum regex_symbol term) {
    return parse_table[nonterm][term];
}

static bool is_terminal(int sym) {
    return sym < REGEX_NT;
}

static bool is_action(int sym) {
    return sym >= DO_REGEX;
}

static void debug_sym_stack(struct array *stack) {
    int sym;

    pdebug("symbol stack: ");
    for (int i = 0; i < asize(stack); i++) {
        at(&sym, i, stack);
        debug_("%s ", str_for_sym(sym));
    }

    debug_("\n");
}

static void debug_result_stack(struct array *results) {
    pdebug("result stack size: %d\n", asize(results));
}

bool parse_regex_nonrec(char *regex, struct parse_context *context) {
    bool success = true;
    struct array
        *stack = init_array(sizeof(enum regex_symbol), PARSE_STACK_SIZE, 0, 0),
        *results = init_array(sizeof(union rval), PARSE_STACK_SIZE, 0, 0);
    enum regex_symbol ssym;

    start_scanning(regex, context);
    push_sym(REGEX_NT, stack);

    while (success && !aempty(stack)) {
        apeek(&ssym, stack);

        debug_sym_stack(stack);

        if (is_terminal(ssym)) {
            if (peek(context, ssym)) {
                if (ssym == SYMBOL_T)
                    push_rval((union rval) { .sym = symbol(context) }, results);

                expect(context, ssym);
                apop(&ssym, stack);
            } else {
                success = false;
            }
        } else if (is_action(ssym)) {
            union rval lval = NULLRVAL;

            if (ssym == DO_ALT || ssym == DO_CAT || ssym == DO_SYMBOL)
                apop(&lval, results);

            do_action(context, ssym, lval);

            if (ssym == DO_EMPTY)
                push_rval(getval(context), results);

            apop(&ssym, stack);
        } else {
            enum regex_symbol la = lookahead(context);
            enum gram_production p = selectp(NTI(ssym), la);

            pdebug("parse_table[%s][%s] = %d\n", str_for_sym(ssym), str_for_sym(la), p);

            if (p) {
                // the top of the tail loop
                if (p == ALT_TAIL_CAT_P || p == CAT_TAIL_FACTOR_P)
                    push_rval(getval(context), results);

                apop(&ssym, stack);
                push_production_symbols(p, stack);
            } else {
                // TODO: make error handling support first/follow sets in error output
                set_parse_error(EOF_T, context);
                success = false;
            }
        }

        debug_result_stack(results);
    };

    free_array(stack);
    free_array(results);

    return success;
}

