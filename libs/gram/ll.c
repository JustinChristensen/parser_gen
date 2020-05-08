#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/base.h>
#include <base/array.h>
#include <base/assert.h>
#include <base/debug.h>
#include <base/bitset.h>
#include <regex/nfa.h>
#include "gram/ll.h"
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"
#include "internal/gen.c"
#include "internal/macros.c"
#include "internal/spec.c"

#define debug(...) debug_ns("gram_ll", __VA_ARGS__);

static bool _oom_error(struct ll_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct ll_error) { .type = GM_LL_OOM_ERROR, .file = file, .col = col }));

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

static bool not_ll_error(struct ll_error *error) {
    prod(error, ((struct ll_error) { .type = GM_LL_NOT_LL1_ERROR }));
    return false;
}

static bool scanner_error(struct ll_error *error, struct regex_error scanerr) {
    prod(error, ((struct ll_error) { .type = GM_LL_SCANNER_ERROR, .scanerr = scanerr }));
    return false;
}

static bool syntax_error(struct ll_error *error, gram_sym_no expected, struct ll_parser_state *state) {
    prod(error, ((struct ll_error) {
        .type = GM_LL_SYNTAX_ERROR,
        .loc = nfa_match_loc(&state->match),
        .actual = state->lookahead,
        .expected = expected
    }));

    return false;
}

struct ll_parser ll_parser(
    struct nfa_context scanner, gram_sym_no **rtable, gram_rule_no **ptable,
    struct gram_stats stats
) {
    return (struct ll_parser) { rtable, ptable, scanner, stats };
}

static gram_rule_no **alloc_parse_table(struct gram_stats stats) {
    size_t xsyms = sizeof (gram_rule_no) * stats.terms * stats.nonterms;

    gram_rule_no **ptable = malloc(sizeof (gram_rule_no *) * stats.nonterms + xsyms);
    if (!ptable) return NULL;

    gram_rule_no *srules = (gram_rule_no *) (ptable + stats.nonterms);

    memset(srules, 0, xsyms);

    for (unsigned i = 0; i < stats.nonterms; i++)
        ptable[i] = srules + (i * stats.terms);

    return ptable;
}

static gram_sym_no NTI(gram_sym_no nt, struct gram_stats const stats) {
    return nt - stats.terms - 1;
}

static gram_sym_no TI(gram_sym_no t) {
    return t - 1;
}

#define PTABLE_STACK 7
static gram_rule_no **parse_table(struct gram_symbol_analysis const *an, struct gram_parser_spec const *spec) {
    struct gram_stats stats = spec->stats;

    gram_rule_no **ptable = alloc_parse_table(stats);
    if (!ptable) return NULL;

    struct gram_symbol *sym = gram_nonterm0(spec);
    while (!gram_symbol_null(sym)) {
        gram_sym_no nt = sym->num;
        gram_sym_no nti = NTI(nt, stats);
        gram_rule_no *r = sym->derives;

        while (*r) {
            gram_sym_no *s = spec->rules[*r];

            while (*s) {
                struct bsiter it = bsiter(an->firsts[*s]);
                gram_sym_no t;
                while (bsnext(&t, &it))
                    ptable[nti][TI(t)] = *r;
                if (!an->nullable[*s]) break;
                s++;
            }

            if (!*s) {
                struct bsiter it = bsiter(an->follows[nt]);
                gram_sym_no t;
                while (bsnext(&t, &it))
                    ptable[nti][TI(t)] = *r;
            }

            r++;
        }

        sym++;
    }

    return ptable;
}

static gram_sym_no **alloc_rule_table(struct gram_stats stats) {
    return malloc(
        sizeof (gram_sym_no *) * nullterm(offs(stats.rules)) +
        sizeof (gram_sym_no) * (stats.rsymbols + stats.rules)
    );
}

static gram_sym_no **rule_table(struct gram_parser_spec const *spec) {
    struct gram_stats stats = spec->stats;
    gram_sym_no **rtable = alloc_rule_table(stats);
    if (!rtable) return NULL;

    // offset the rule table by 1
    rtable[0] = NULL;

    // start counting 1 past
    gram_sym_no **trule = &rtable[1];
    // symbol lists start after the list pointers
    gram_sym_no *tsym = (gram_sym_no *) (rtable + nullterm(offs(stats.rules)));

    gram_sym_no **r = gram_rule0(spec);
    while (*r) {
        *trule++ = tsym;

        gram_sym_no *s = *r + rulesize(*r);
        while (s != *r) *tsym++ = *--s;
        *tsym++ = 0;

        r++;
    }

    // null-terminate
    *trule = NULL;

    return rtable;
}

bool gen_ll(struct ll_error *error, struct ll_parser *parser, struct gram_parser_spec *spec) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);

    prod(error, ((struct ll_error) { 0 }));
    *parser = (struct ll_parser) { 0 };

    struct gram_stats stats = spec->stats;

    struct gram_symbol_analysis san = { 0 };
    struct gram_analysis gan = { 0 };
    struct nfa_context scanner = { 0 };
    gram_rule_no **ptable = NULL;
    gram_sym_no **rtable = NULL;

    if (!gram_analyze_symbols(&san, spec))
        return false;

    if (!gram_analyze(&gan, &san, spec))
        return oom_error(error, NULL);

    if (gan.clas < GM_LL) {
        not_ll_error(error);
        goto free;
    }

    free_gram_analysis(&gan);

    if (!init_scanner(&scanner, spec->patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    rtable = rule_table(spec);
    if (!rtable) {
        oom_error(error, NULL);
        goto free;
    }

    ptable = parse_table(&san, spec);
    if (!ptable) {
        oom_error(error, NULL);
        goto free;
    }

    free_gram_symbol_analysis(&san);

    *parser = ll_parser(scanner, rtable, ptable, stats);

    return true;
free:
    free_gram_symbol_analysis(&san);
    free_gram_analysis(&gan);
    free_nfa_context(&scanner);
    free(ptable);
    free(rtable);

    return false;
}

void print_ll_parser(FILE *handle, struct ll_parser *parser) {
    assert(parser != NULL);

    struct gram_stats const stats = parser->stats;

    fprintf(handle, "rule table:\n\n");
    gram_sym_no **rtable = parser->rtable;
    FOR_RULE(stats, r) {
        gram_sym_no *s = rtable[r];
        fprintf(handle, "  %d. ", r);
        if (s) while (*s) fprintf(handle, "%u ", *s), s++;
        fprintf(handle, "\n");
    }
    fprintf(handle, "\n");

    fprintf(handle, "parse table:\n\n");
    gram_rule_no **ptable = parser->ptable;
    for (unsigned n = 0; n < stats.nonterms; n++) {
        for (unsigned t = 0; t < stats.terms; t++) {
            if (ptable[n][t]) {
                fprintf(handle, "  ptable[%u][%u] = %u\n", n + stats.terms + 1, t + 1, ptable[n][t]);
            }
        }
    }
    fprintf(handle, "\n");
}

void free_ll_parser(struct ll_parser *parser) {
    if (!parser) return;
    free(parser->rtable);
    free(parser->ptable);
    free_nfa_context(&parser->scanner);
    *parser = (struct ll_parser) { 0 };
}

struct ll_parser_state ll_parser_state(struct ll_parser *parser) {
    assert(parser != NULL);
    return (struct ll_parser_state) { .parser = parser };
}

static void push_rule(gram_rule_no r, gram_sym_no **rtable, struct array *syms) {
    gram_sym_no *s = rtable[r];
    while (*s) apush(s, syms), s++;
};

static bool start_scanning(char *input, struct ll_parser_state *state) {
    if (nfa_start_match(input, &state->match, &state->parser->scanner)) {
        state->lookahead = nfa_match(&state->match);
        debug("initial lookahead: %u\n", state->lookahead);
        return true;
    }

    return false;
}

static bool peek(gram_sym_no expected, struct ll_parser_state *state) {
    return state->lookahead == expected;
}

static bool expect(gram_sym_no expected, struct ll_parser_state *state) {
    if (peek(expected, state)) {
        debug("success: expected %u, actual %u\n", expected, state->lookahead);
        state->lookahead = nfa_match(&state->match);
        return true;
    }

    debug("failure: expected %u, actual %u\n", expected, state->lookahead);

    return false;
}

static void debug_syms(struct array *syms) {
    int sym;

    debug("symbols: ");
    for (unsigned i = 0, ssize = asize(syms); i < ssize; i++) {
        at(&sym, i, syms);
        debug("%u ", sym);
    }

    debug("\n");
}

#define SYM_STACK_SIZE 7
bool ll_parse(struct ll_error *error, char *input, struct ll_parser_state *state) {
    assert(state != NULL);

    if (!start_scanning(input, state)) return oom_error(error, NULL);

    struct array *syms = init_array(sizeof (gram_sym_no), SYM_STACK_SIZE, 0, 0);
    if (!syms) return oom_error(error, NULL);

    struct ll_parser const *parser = state->parser;
    gram_sym_no **rtable = parser->rtable;
    gram_rule_no **ptable = parser->ptable;

    struct gram_stats stats = parser->stats;
    gram_sym_no const nonterm0 = offs(stats.terms);

    push_rule(GM_START, rtable, syms);

    bool success = true;
    gram_sym_no sym = 0;

    while (success && !aempty(syms)) {
        apeek(&sym, syms);
        debug_syms(syms);

        if (state->lookahead == RX_REJECTED) {
            success = syntax_error(error, sym, state);
            break;
        }

        bool const isterm = sym < nonterm0;

        if (isterm) {
            if (expect(sym, state)) apop(&sym, syms);
            else (success = syntax_error(error, sym, state));
        } else {
            debug("ptable[%u][%u]", sym, state->lookahead);
            gram_rule_no rule = ptable[NTI(sym, stats)][TI(state->lookahead)];
            debug(" = %u\n", rule);

            if (rule) {
                printf("parsing... %u\n", sym);
                apop(&sym, syms);
                push_rule(rule, rtable, syms);
            } else {
                success = syntax_error(error, sym, state);
            }
        }
    }

    free_array(syms);

    return success;
}

void free_ll_parser_state(struct ll_parser_state *state) {
    free_nfa_match(&state->match);
}

#define ERROR_FMT_END "\n|\n"
#define OOM_ERROR_FMT_START "| LL Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
#define NOT_LL1_FMT "| Grammar Not LL(1)\n|\n"
#define SYNTAX_ERROR_FMT_START "| LL Syntax Error\n|\n| Got: %u\n| Expected: %u"
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
void print_ll_error(FILE *handle, struct ll_error error) {
    switch (error.type) {
        case GM_LL_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, error.actual, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_LL_NOT_LL1_ERROR:
            fprintf(handle, NOT_LL1_FMT);
            break;
        case GM_LL_SCANNER_ERROR:
            print_regex_error(handle, error.scanerr);
            break;
        case GM_LL_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
    }
}

