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
        [EOF_T] = REGEX_EXPR_P,
        [CHAR_T] = REGEX_EXPR_P,
        [ALT_T] = REGEX_EXPR_P,
        [DOTALL_T] = REGEX_EXPR_P,
        [LPAREN_T] = REGEX_EXPR_P,
        [CLASS_T] = REGEX_EXPR_P,
        [NEG_CLASS_T] = REGEX_EXPR_P,
        [TAG_BRACE_T] = REGEX_EXPR_P
    },
    [NTI(EXPR_NT)] = {
        [EOF_T] = EXPR_EMPTY_P,
        [CHAR_T] = EXPR_ALT_P,
        [ALT_T] = EXPR_ALT_P,
        [DOTALL_T] = EXPR_ALT_P,
        [LPAREN_T] = EXPR_ALT_P,
        [RPAREN_T] = EXPR_EMPTY_P,
        [CLASS_T] = EXPR_ALT_P,
        [NEG_CLASS_T] = EXPR_ALT_P,
        [TAG_BRACE_T] = EXPR_ALT_P
    },
    [NTI(ALT_NT)] = {
        [EOF_T] = ALT_EMPTY_P,
        [CHAR_T] = ALT_FACTOR_P,
        [ALT_T] = ALT_EMPTY_P,
        [DOTALL_T] = ALT_FACTOR_P,
        [LPAREN_T] = ALT_FACTOR_P,
        [RPAREN_T] = ALT_EMPTY_P,
        [CLASS_T] = ALT_FACTOR_P,
        [NEG_CLASS_T] = ALT_FACTOR_P,
        [TAG_BRACE_T] = ALT_FACTOR_P
    },
    [NTI(ALTS_NT)] = {
        [EOF_T] = EMPTY_P,
        [ALT_T] = ALTS_ALT_P,
        [RPAREN_T] = EMPTY_P
    },
    [NTI(FACTOR_NT)] = {
        [CHAR_T] = FACTOR_CHAR_P,
        [DOTALL_T] = FACTOR_DOTALL_P,
        [LPAREN_T] = FACTOR_SUBEXPR_P,
        [CLASS_T] = FACTOR_CLASS_P,
        [NEG_CLASS_T] = FACTOR_NEG_CLASS_P,
        [TAG_BRACE_T] = FACTOR_TAG_P
    },
    [NTI(FACTORS_NT)] = {
        [EOF_T] = EMPTY_P,
        [CHAR_T] = FACTORS_FACTOR_P,
        [ALT_T] = EMPTY_P,
        [DOTALL_T] = FACTORS_FACTOR_P,
        [LPAREN_T] = FACTORS_FACTOR_P,
        [RPAREN_T] = EMPTY_P,
        [CLASS_T] = FACTORS_FACTOR_P,
        [NEG_CLASS_T] = FACTORS_FACTOR_P,
        [TAG_BRACE_T] = FACTORS_FACTOR_P
    },
    [NTI(CHAR_CLASS_NT)] = {
        [RANGE_T] = CHAR_CLASS_RANGES_P,
        [END_CLASS_T] = CHAR_CLASS_RBRACKET_P
    },
    [NTI(RANGES_NT)] = {
        [RANGE_T] = RANGES_RANGE_P,
        [END_CLASS_T] = EMPTY_P
    },
    [NTI(UNOPS_NT)] = {
        [EOF_T] = EMPTY_P,
        [CHAR_T] = EMPTY_P,
        [ALT_T] = EMPTY_P,
        [STAR_T] = UNOPS_STAR_P,
        [PLUS_T] = UNOPS_PLUS_P,
        [OPTIONAL_T] = UNOPS_OPTIONAL_P,
        [DOTALL_T] = EMPTY_P,
        [LPAREN_T] = EMPTY_P,
        [RPAREN_T] = EMPTY_P,
        [CLASS_T] = EMPTY_P,
        [NEG_CLASS_T] = EMPTY_P,
        [TAG_BRACE_T] = EMPTY_P,
        [LBRACE_T] = UNOPS_REPEAT_EXACT_P
    }
};

#define SYMS (enum regex_symbol[])
static enum regex_symbol *rule_table[] = {
    [ERROR_P] =                SYMS { 0 },
    [EMPTY_P] =                SYMS { 0 },
    [REGEX_EXPR_P] =           SYMS { DO_REGEX, EOF_T, EXPR_NT, 0 },
    [EXPR_EMPTY_P] =           SYMS { DO_EMPTY, 0 },
    [EXPR_ALT_P] =             SYMS { ALTS_NT, ALT_NT, 0 },
    [ALT_FACTOR_P] =           SYMS { FACTORS_NT, DO_EMPTY, 0 },
    [ALT_EMPTY_P] =            SYMS { DO_EMPTY, 0 },
    [ALTS_ALT_P] =             SYMS { ALTS_NT, DO_ALT, ALT_NT, ALT_T, 0 },
    [FACTORS_FACTOR_P] =       SYMS { FACTORS_NT, DO_CAT, FACTOR_NT, 0 },
    [CHAR_CLASS_RBRACKET_P] =  SYMS { DO_CHAR_CLASS, END_CLASS_T, 0 },
    [CHAR_CLASS_RANGES_P] =    SYMS { DO_CHAR_CLASS, END_CLASS_T, RANGES_NT, DO_RANGES, RANGE_T, 0 },
    [RANGES_RANGE_P] =         SYMS { RANGES_NT, DO_RANGE, RANGE_T, 0 },
    [FACTOR_SUBEXPR_P] =       SYMS { UNOPS_NT, DO_SUB, RPAREN_T, EXPR_NT, LPAREN_T, 0 },
    [FACTOR_TAG_P] =           SYMS { UNOPS_NT, DO_TAG, RBRACE_T, TAG_T, TAG_BRACE_T, 0 },
    [FACTOR_CLASS_P] =         SYMS { UNOPS_NT, CHAR_CLASS_NT, CLASS_T, 0 },
    [FACTOR_NEG_CLASS_P] =     SYMS { UNOPS_NT, DO_NEG_CLASS, CHAR_CLASS_NT, NEG_CLASS_T, 0 },
    [FACTOR_DOTALL_P] =        SYMS { UNOPS_NT, DO_DOTALL, DOTALL_T, 0 },
    [FACTOR_CHAR_P] =          SYMS { UNOPS_NT, DO_CHAR, CHAR_T, 0 },
    [UNOPS_STAR_P] =           SYMS { UNOPS_NT, DO_STAR, STAR_T, 0 },
    [UNOPS_PLUS_P] =           SYMS { UNOPS_NT, DO_PLUS, PLUS_T, 0 },
    [UNOPS_OPTIONAL_P] =       SYMS { UNOPS_NT, DO_OPTIONAL, OPTIONAL_T, 0 },
    [UNOPS_REPEAT_EXACT_P] =   SYMS { UNOPS_NT, DO_REPEAT_EXACT, RBRACE_T, NUM_T, LBRACE_T, 0 },
};

static void push_sym(int sym, struct array *stack) { apush(&sym, stack); }
static void push_result(union regex_result val, struct array *results) { apush(&val, results); }

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

static int psize = 0;
static void debug_result_stack(struct array *results) {
    int csize = asize(results);

    if (csize != psize) {
        pdebug("result stack size: %d\n", csize);
        psize = csize;
    }
}

bool parse_regex_nonrec(char *regex, struct parse_context *context) {
    bool success = true;
    struct array
        *stack = init_array(sizeof(enum regex_symbol), PARSE_STACK_SIZE, 0, 0),
        *results = init_array(sizeof(union regex_result), PARSE_STACK_SIZE, 0, 0);
    enum regex_symbol ssym;
    char tagbuf[BUFSIZ] = "";

    start_scanning(regex, context);
    push_sym(REGEX_NT, stack);

    while (success && !aempty(stack)) {
        apeek(&ssym, stack);

        debug_sym_stack(stack);

        if (is_terminal(ssym)) {
            if (peek(ssym, context)) {
                if (ssym == CHAR_T || ssym == RANGE_T || ssym == NUM_T)
                    push_result(lookahead_val(context), results);
                else if (ssym == TAG_T) {
                    push_result(tag_val(tagbuf, context), results);
                }

                expect(ssym, context);
                apop(&ssym, stack);
            } else {
                success = set_syntax_error(ssym, context);
            }
        } else if (is_action(ssym)) {
            union regex_result val = NULLRVAL;

            // actions that use local variables in the
            // recursive routines
            switch (ssym) {
                case DO_ALT:
                case DO_CAT:
                case DO_CHAR:
                case DO_RANGES:
                case DO_RANGE:
                case DO_CHAR_CLASS:
                case DO_TAG:
                case DO_REPEAT_EXACT:
                    apop(&val, results);
                    break;
                default:
                    break;
            }

            success = do_action(ssym, val, context);

            // push the last result onto the result stack
            if (ssym == DO_RANGES)
                push_result(get_result(context), results);

            apop(&ssym, stack);
        } else {
            enum regex_symbol la = lookahead(context);
            enum gram_production p = selectp(NTI(ssym), la);

            pdebug("parse_table[%s][%s] = %s\n", str_for_sym(ssym), str_for_sym(la), str_for_prod(p));

            if (p) {
                // the top of the tail loop
                if (p == ALTS_ALT_P || p == FACTORS_FACTOR_P)
                    push_result(get_result(context), results);
                else if (p == CHAR_CLASS_RBRACKET_P)
                    push_result(NULLRVAL, results);

                apop(&ssym, stack);
                push_production_symbols(p, stack);
            } else {
                success = set_syntax_error(ssym, context);
            }
        }

        debug_result_stack(results);
    };

    free_array(stack);
    free_array(results);

    return success;
}

