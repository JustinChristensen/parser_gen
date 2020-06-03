#ifndef REGEX_PARSER_NONREC_C_
#define REGEX_PARSER_NONREC_C_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <base/array.h>
#include "regex/base.h"

#define PARSE_STACK_SIZE 7

enum regex_production {
    ERROR_P,

    // $empty
    EMPTY_P,

    // regex = expr eof { regex };
    REGEX_EXPR_P,

    // expr = { empty } $empty;
    EXPR_EMPTY_P,
    // expr = alt alts;
    EXPR_ALT_P,

    // alt = { empty } factors;
    ALT_FACTOR_P,
    // alt = { empty } $empty;
    ALT_EMPTY_P,

    // alts  = '|' alt { alt } alts:
    ALTS_ALT_P,

    // factors = factor { cat } factors;
    FACTORS_FACTOR_P,

    // char_class = ']' { char_class };
    CHAR_CLASS_RBRACKET_P,

    // char_class = 'a-z' { range_head } ranges ']' { char_class };
    CHAR_CLASS_RANGES_P,

    // ranges      = 'a-z' { range } ranges
    RANGES_RANGE_P,

    // factor = '(' expr ')' { sub } unops;
    FACTOR_SUBEXPR_P,
    // factor = '{' tag '}' { tag } unops;
    FACTOR_TAG_P,
    // factor = '[' char_class unops;
    FACTOR_CLASS_P,
    // factor = '[^' char_class { neg_char_class } unops;
    FACTOR_NEG_CLASS_P,
    // factor = '.' { dotall } unops;
    FACTOR_DOTALL_P,
    // factor = a { char } unops;;
    FACTOR_CHAR_P,

    // unops = '*' { star } unops;
    UNOPS_STAR_P,
    // unops = '+' { plus } unops;
    UNOPS_PLUS_P,
    // unops = '?' { optional } unops;
    UNOPS_OPTIONAL_P,
    // unops = '{' number '}' { repeat_exact } unops;
    UNOPS_REPEAT_EXACT_P
};

static enum regex_production parse_table[NUM_NONTERMINALS][NUM_TERMINALS] = {
    [NTI(RX_REGEX_NT)] = {
        [RX_EOF_T] = REGEX_EXPR_P,
        [RX_CHAR_T] = REGEX_EXPR_P,
        [RX_ALT_T] = REGEX_EXPR_P,
        [RX_DOTALL_T] = REGEX_EXPR_P,
        [RX_LPAREN_T] = REGEX_EXPR_P,
        [RX_CLASS_T] = REGEX_EXPR_P,
        [RX_NEG_CLASS_T] = REGEX_EXPR_P,
        [RX_TAG_BRACE_T] = REGEX_EXPR_P
    },
    [NTI(RX_EXPR_NT)] = {
        [RX_EOF_T] = EXPR_EMPTY_P,
        [RX_CHAR_T] = EXPR_ALT_P,
        [RX_ALT_T] = EXPR_ALT_P,
        [RX_DOTALL_T] = EXPR_ALT_P,
        [RX_LPAREN_T] = EXPR_ALT_P,
        [RX_RPAREN_T] = EXPR_EMPTY_P,
        [RX_CLASS_T] = EXPR_ALT_P,
        [RX_NEG_CLASS_T] = EXPR_ALT_P,
        [RX_TAG_BRACE_T] = EXPR_ALT_P
    },
    [NTI(RX_ALT_NT)] = {
        [RX_EOF_T] = ALT_EMPTY_P,
        [RX_CHAR_T] = ALT_FACTOR_P,
        [RX_ALT_T] = ALT_EMPTY_P,
        [RX_DOTALL_T] = ALT_FACTOR_P,
        [RX_LPAREN_T] = ALT_FACTOR_P,
        [RX_RPAREN_T] = ALT_EMPTY_P,
        [RX_CLASS_T] = ALT_FACTOR_P,
        [RX_NEG_CLASS_T] = ALT_FACTOR_P,
        [RX_TAG_BRACE_T] = ALT_FACTOR_P
    },
    [NTI(RX_ALTS_NT)] = {
        [RX_EOF_T] = EMPTY_P,
        [RX_ALT_T] = ALTS_ALT_P,
        [RX_RPAREN_T] = EMPTY_P
    },
    [NTI(RX_FACTOR_NT)] = {
        [RX_CHAR_T] = FACTOR_CHAR_P,
        [RX_DOTALL_T] = FACTOR_DOTALL_P,
        [RX_LPAREN_T] = FACTOR_SUBEXPR_P,
        [RX_CLASS_T] = FACTOR_CLASS_P,
        [RX_NEG_CLASS_T] = FACTOR_NEG_CLASS_P,
        [RX_TAG_BRACE_T] = FACTOR_TAG_P
    },
    [NTI(RX_FACTORS_NT)] = {
        [RX_EOF_T] = EMPTY_P,
        [RX_CHAR_T] = FACTORS_FACTOR_P,
        [RX_ALT_T] = EMPTY_P,
        [RX_DOTALL_T] = FACTORS_FACTOR_P,
        [RX_LPAREN_T] = FACTORS_FACTOR_P,
        [RX_RPAREN_T] = EMPTY_P,
        [RX_CLASS_T] = FACTORS_FACTOR_P,
        [RX_NEG_CLASS_T] = FACTORS_FACTOR_P,
        [RX_TAG_BRACE_T] = FACTORS_FACTOR_P
    },
    [NTI(RX_CHAR_CLASS_NT)] = {
        [RX_RANGE_T] = CHAR_CLASS_RANGES_P,
        [RX_END_CLASS_T] = CHAR_CLASS_RBRACKET_P
    },
    [NTI(RX_RANGES_NT)] = {
        [RX_RANGE_T] = RANGES_RANGE_P,
        [RX_END_CLASS_T] = EMPTY_P
    },
    [NTI(RX_UNOPS_NT)] = {
        [RX_EOF_T] = EMPTY_P,
        [RX_CHAR_T] = EMPTY_P,
        [RX_ALT_T] = EMPTY_P,
        [RX_STAR_T] = UNOPS_STAR_P,
        [RX_PLUS_T] = UNOPS_PLUS_P,
        [RX_OPTIONAL_T] = UNOPS_OPTIONAL_P,
        [RX_DOTALL_T] = EMPTY_P,
        [RX_LPAREN_T] = EMPTY_P,
        [RX_RPAREN_T] = EMPTY_P,
        [RX_CLASS_T] = EMPTY_P,
        [RX_NEG_CLASS_T] = EMPTY_P,
        [RX_TAG_BRACE_T] = EMPTY_P,
        [RX_LBRACE_T] = UNOPS_REPEAT_EXACT_P
    }
};

#define SYMS (enum regex_symbol[])
static enum regex_symbol *rule_table[] = {
    [ERROR_P] =                SYMS { 0 },
    [EMPTY_P] =                SYMS { 0 },
    [REGEX_EXPR_P] =           SYMS { RX_DO_REGEX, RX_EOF_T, RX_EXPR_NT, 0 },
    [EXPR_EMPTY_P] =           SYMS { RX_DO_EMPTY, 0 },
    [EXPR_ALT_P] =             SYMS { RX_ALTS_NT, RX_ALT_NT, 0 },
    [ALT_FACTOR_P] =           SYMS { RX_FACTORS_NT, RX_DO_EMPTY, 0 },
    [ALT_EMPTY_P] =            SYMS { RX_DO_EMPTY, 0 },
    [ALTS_ALT_P] =             SYMS { RX_ALTS_NT, RX_DO_ALT, RX_ALT_NT, RX_ALT_T, 0 },
    [FACTORS_FACTOR_P] =       SYMS { RX_FACTORS_NT, RX_DO_CAT, RX_FACTOR_NT, 0 },
    [CHAR_CLASS_RBRACKET_P] =  SYMS { RX_DO_CHAR_CLASS, RX_END_CLASS_T, 0 },
    [CHAR_CLASS_RANGES_P] =    SYMS { RX_DO_CHAR_CLASS, RX_END_CLASS_T, RX_RANGES_NT, RX_DO_RANGES, RX_RANGE_T, 0 },
    [RANGES_RANGE_P] =         SYMS { RX_RANGES_NT, RX_DO_RANGE, RX_RANGE_T, 0 },
    [FACTOR_SUBEXPR_P] =       SYMS { RX_UNOPS_NT, RX_DO_SUB, RX_RPAREN_T, RX_EXPR_NT, RX_LPAREN_T, 0 },
    [FACTOR_TAG_P] =           SYMS { RX_UNOPS_NT, RX_DO_TAG, RX_RBRACE_T, RX_TAG_T, RX_TAG_BRACE_T, 0 },
    [FACTOR_CLASS_P] =         SYMS { RX_UNOPS_NT, RX_CHAR_CLASS_NT, RX_CLASS_T, 0 },
    [FACTOR_NEG_CLASS_P] =     SYMS { RX_UNOPS_NT, RX_DO_NEG_CLASS, RX_CHAR_CLASS_NT, RX_NEG_CLASS_T, 0 },
    [FACTOR_DOTALL_P] =        SYMS { RX_UNOPS_NT, RX_DO_DOTALL, RX_DOTALL_T, 0 },
    [FACTOR_CHAR_P] =          SYMS { RX_UNOPS_NT, RX_DO_CHAR, RX_CHAR_T, 0 },
    [UNOPS_STAR_P] =           SYMS { RX_UNOPS_NT, RX_DO_STAR, RX_STAR_T, 0 },
    [UNOPS_PLUS_P] =           SYMS { RX_UNOPS_NT, RX_DO_PLUS, RX_PLUS_T, 0 },
    [UNOPS_OPTIONAL_P] =       SYMS { RX_UNOPS_NT, RX_DO_OPTIONAL, RX_OPTIONAL_T, 0 },
    [UNOPS_REPEAT_EXACT_P] =   SYMS { RX_UNOPS_NT, RX_DO_REPEAT_EXACT, RX_RBRACE_T, RX_NUM_T, RX_LBRACE_T, 0 },
};

static void debug_sym_stack(struct array *stack) {
    int sym;

    pdebug("symbol stack: ");
    for (unsigned i = 0; i < asize(stack); i++) {
        at(&sym, i, stack);
        pdebug("%s ", str_for_regex_sym(sym));
    }

    pdebug("\n");
}

static void debug_result_stack(struct array *results, int *psize) {
    int csize = asize(results);

    if (csize != *psize) {
        pdebug("result stack size: %d\n", csize);
        *psize = csize;
    }
}


static void push_sym(int sym, struct array *stack) { apush(&sym, stack); }
static void push_result(union regex_result val, struct array *results) { apush(&val, results); }

static void push_production_symbols(enum regex_production production, struct array *stack) {
    enum regex_symbol *sym = rule_table[production];
    while (*sym) push_sym(*sym, stack), sym++;
}

static enum regex_production selectp(int nonterm, enum regex_symbol term) {
    return parse_table[nonterm][term];
}

static char const *str_for_prod(enum regex_production p) {
    switch (p) {
        case ERROR_P:               return "ERROR_P";
        case EMPTY_P:               return "EMPTY_P";
        case REGEX_EXPR_P:          return "REGEX_EXPR_P";
        case EXPR_EMPTY_P:          return "EXPR_EMPTY_P";
        case EXPR_ALT_P:            return "EXPR_ALT_P";
        case ALT_FACTOR_P:          return "ALT_FACTOR_P";
        case ALT_EMPTY_P:           return "ALT_EMPTY_P";
        case ALTS_ALT_P:            return "ALTS_ALT_P";
        case FACTORS_FACTOR_P:      return "FACTORS_FACTOR_P";
        case CHAR_CLASS_RBRACKET_P: return "CHAR_CLASS_RBRACKET_P";
        case CHAR_CLASS_RANGES_P:   return "CHAR_CLASS_RANGES_P";
        case RANGES_RANGE_P:        return "RANGES_RANGE_P";
        case FACTOR_SUBEXPR_P:      return "FACTOR_SUBEXPR_P";
        case FACTOR_TAG_P:          return "FACTOR_TAG_P";
        case FACTOR_CLASS_P:        return "FACTOR_CLASS_P";
        case FACTOR_NEG_CLASS_P:    return "FACTOR_NEG_CLASS_P";
        case FACTOR_DOTALL_P:       return "FACTOR_DOTALL_P";
        case FACTOR_CHAR_P:         return "FACTOR_CHAR_P";
        case UNOPS_STAR_P:          return "UNOPS_STAR_P";
        case UNOPS_PLUS_P:          return "UNOPS_PLUS_P";
        case UNOPS_OPTIONAL_P:      return "UNOPS_OPTIONAL_P";
        case UNOPS_REPEAT_EXACT_P:  return "UNOPS_REPEAT_EXACT_P";
    }
}

static bool is_terminal(int sym) { return sym < RX_REGEX_NT; }
static bool is_action(int sym) { return sym >= RX_DO_REGEX; }
static bool parse_regex_nonrec(char *pattern, struct regex_loc loc, struct regex_parse_context *context) {
    bool success = true;
    struct array
        *stack = init_array(sizeof(enum regex_symbol), PARSE_STACK_SIZE, 0, 0),
        *results = init_array(sizeof(union regex_result), PARSE_STACK_SIZE, 0, 0);
    enum regex_symbol ssym;
    char tagbuf[BUFSIZ] = "";
    int prevres = 0;

    start_scanning(pattern, loc, context);
    push_sym(RX_REGEX_NT, stack);

    while (success && !aempty(stack)) {
        apeek(&ssym, stack);

        debug_sym_stack(stack);

        if (is_terminal(ssym)) {
            if (peek(ssym, context)) {
                if (ssym == RX_CHAR_T || ssym == RX_RANGE_T || ssym == RX_NUM_T)
                    push_result(lookahead_val(context), results);
                else if (ssym == RX_TAG_T) {
                    push_result(tag_val(tagbuf, context), results);
                }

                expect(ssym, context);
                apop(&ssym, stack);
            } else {
                success = set_syntax_error(ssym, context);
            }
        } else if (is_action(ssym)) {
            union regex_result val = RX_NULL_RESULT;

            // actions that use local variables in the
            // recursive routines
            switch (ssym) {
                case RX_DO_ALT:
                case RX_DO_CAT:
                case RX_DO_CHAR:
                case RX_DO_RANGES:
                case RX_DO_RANGE:
                case RX_DO_CHAR_CLASS:
                case RX_DO_TAG:
                case RX_DO_REPEAT_EXACT:
                    apop(&val, results);
                    break;
                default:
                    break;
            }

            success = do_action(ssym, val, context);

            // push the last result onto the result stack
            if (ssym == RX_DO_RANGES)
                push_result(get_result(context), results);

            apop(&ssym, stack);
        } else {
            enum regex_symbol la = lookahead(context);
            enum regex_production p = selectp(NTI(ssym), la);

            pdebug("parse_table[%s][%s] = %s\n", str_for_regex_sym(ssym), str_for_regex_sym(la), str_for_prod(p));

            if (p) {
                // the top of the tail loop
                if (p == ALTS_ALT_P || p == FACTORS_FACTOR_P)
                    push_result(get_result(context), results);
                else if (p == CHAR_CLASS_RBRACKET_P)
                    push_result(RX_NULL_RESULT, results);

                apop(&ssym, stack);
                push_production_symbols(p, stack);
            } else {
                success = set_syntax_error(ssym, context);
            }
        }

        debug_result_stack(results, &prevres);
    };

    free_array(stack);
    free_array(results);

    return success;
}

#endif // REGEX_PARSER_NONREC_C_
