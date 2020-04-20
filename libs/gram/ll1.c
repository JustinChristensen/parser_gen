#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/base.h>
#include <base/array.h>
#include <base/assert.h>
#include <base/debug.h>
#include <base/bitset.h>
#include <regex/nfa.h>
#include "gram/ll1.h"
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"

#define debug(...) debug_ns("gram_ll1", __VA_ARGS__);

static bool _oom_error(struct ll1_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct ll1_error) { .type = GM_LL1_OOM_ERROR, .file = file, .col = col }));

    return false;
}

static bool not_ll_error(struct ll1_error *error) {
    prod(error, ((struct ll1_error) { .type = GM_LL1_NOT_LL1_ERROR }));
    return false;
}

static bool scanner_error(struct ll1_error *error, struct regex_error scanerr) {
    prod(error, ((struct ll1_error) { .type = GM_LL1_SCANNER_ERROR, .scanerr = scanerr }));
    return false;
}

static bool syntax_error(struct ll1_error *error, unsigned expected, struct ll1_parser_state *state) {
    prod(error, ((struct ll1_error) {
        .type = GM_LL1_SYNTAX_ERROR,
        .loc = nfa_match_loc(&state->match),
        .actual = state->lookahead,
        .expected = expected
    }));

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

#define nullterm(n) ((n) + 1)
#define offs(n) ((n) + 1)

struct ll1_parser ll1_parser(
    struct nfa_context scanner, unsigned **rtable, unsigned **ptable,
    struct gram_stats stats
) {
    return (struct ll1_parser) { rtable, ptable, scanner, stats };
}

static unsigned **alloc_parse_table(struct gram_stats stats) {
    size_t xsyms = sizeof (unsigned) * stats.terms * stats.nonterms;

    unsigned **ptable = malloc(sizeof (unsigned *) * stats.nonterms + xsyms);
    if (!ptable) return NULL;

    unsigned *srules = (unsigned *) (ptable + stats.nonterms);

    memset(srules, 0, xsyms);

    for (int i = 0; i < stats.nonterms; i++)
        ptable[i] = srules + (i * stats.terms);

    return ptable;
}

static unsigned NTI(unsigned nt, struct gram_stats const stats) {
    return nt - stats.terms - 1;
}

static unsigned TI(unsigned t) {
    return t - 1;
}

#define PTABLE_STACK 7
static unsigned **parse_table(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    struct gram_stats stats = spec->stats;

    unsigned **ptable = alloc_parse_table(stats);
    if (!ptable) return NULL;

    struct gram_symbol *sym = gram_nonterm0(spec);
    while (!gram_symbol_null(sym)) {
        unsigned nt = sym->num;
        unsigned nti = NTI(nt, stats);
        unsigned *r = sym->derives;

        while (*r) {
            unsigned *s = spec->rules[*r];

            while (*s) {
                struct bsiter it = bsiter(an->firsts[*s]);
                unsigned t;
                while (bsnext(&t, &it))
                    ptable[nti][TI(t)] = *r;
                if (!an->nullable[*s]) break;
                s++;
            }

            if (!*s) {
                struct bsiter it = bsiter(an->follows[nt]);
                unsigned t;
                while (bsnext(&t, &it))
                    ptable[nti][TI(t)] = *r;
            }

            r++;
        }

        sym++;
    }

    return ptable;
}

static unsigned **alloc_rule_table(struct gram_stats stats) {
    return malloc(
        sizeof (unsigned *) * nullterm(offs(stats.rules)) +
        sizeof (unsigned) * (stats.rsymbols + stats.rules)
    );
}

static unsigned rsize(unsigned *s) {
    unsigned size = 0;
    while (*s++) size++;
    return size;
}

static unsigned **rule_table(struct gram_parser_spec const *spec) {
    struct gram_stats stats = spec->stats;
    unsigned **rtable = alloc_rule_table(stats);
    if (!rtable) return NULL;

    // offset the rule table by 1
    rtable[0] = NULL;

    // start counting 1 past
    unsigned **trule = &rtable[1];
    // symbol lists start after the list pointers
    unsigned *tsym = (unsigned *) (rtable + nullterm(offs(stats.rules)));

    unsigned **r = gram_rule0(spec);
    while (*r) {
        *trule++ = tsym;

        unsigned *s = *r + rsize(*r);
        while (s != *r) *tsym++ = *--s;
        *tsym++ = 0;

        r++;
    }

    // null-terminate
    *trule = NULL;

    return rtable;
}

static struct regex_pattern const default_tagged_patterns[] = {
    RX_ALPHA(RX_TAG_ONLY), RX_ALPHA_(RX_TAG_ONLY),
    RX_ALNUM(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
    RX_SPACE(RX_TAG_ONLY),
    RX_END_PATTERNS
};

bool gen_ll1(struct ll1_error *error, struct ll1_parser *parser, struct gram_parser_spec *spec) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);

    prod(error, ((struct ll1_error) { 0 }));
    *parser = (struct ll1_parser) { 0 };

    struct gram_stats stats = spec->stats;

    struct gram_symbol_analysis san = { 0 };
    struct nfa_context scanner = { 0 };
    unsigned **ptable = NULL, **rtable = NULL;

    if (!gram_analyze_symbols(&san, spec))
        return false;

    // TODO: figure out what to do about heap allocated errors
    struct gram_analysis gan = { 0 };
    if (!gram_analyze(&gan, &san, spec)) {
        oom_error(error, NULL);
        goto free;
    }

    if (gan.clas < GM_LL) {
        not_ll_error(error);
        goto free;
    }

    if (!nfa_context(&scanner, default_tagged_patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    if (!nfa_add_patterns(spec->patterns, &scanner)) {
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

    *parser = ll1_parser(scanner, rtable, ptable, stats);

    return true;
free:
    free_gram_analysis(&gan);
    free_gram_symbol_analysis(&san);
    free_nfa_context(&scanner);
    free(ptable);
    free(rtable);

    return false;
}

void print_ll1_parser(FILE *handle, struct ll1_parser *parser) {
    assert(parser != NULL);

    struct gram_stats stats = parser->stats;

    fprintf(handle, "rule table:\n\n");
    unsigned **rtable = parser->rtable;
    for (int r = 0; r < nullterm(offs(stats.rules)); r++) {
        unsigned *s = rtable[r];
        fprintf(handle, "  %d. ", r);
        if (s) while (*s) fprintf(handle, "%u ", *s), s++;
        fprintf(handle, "\n");
    }
    fprintf(handle, "\n");

    fprintf(handle, "parse table:\n\n");
    unsigned **ptable = parser->ptable;
    for (int n = 0; n < stats.nonterms; n++) {
        for (int t = 0; t < stats.terms; t++) {
            if (ptable[n][t]) {
                fprintf(handle, "  ptable[%u][%u] = %u\n", n + stats.terms + 1, t + 1, ptable[n][t]);
            }
        }
    }
    fprintf(handle, "\n");
}

void free_ll1_parser(struct ll1_parser *parser) {
    if (!parser) return;
    free(parser->rtable);
    free(parser->ptable);
    free_nfa_context(&parser->scanner);
    *parser = (struct ll1_parser) { 0 };
}

struct ll1_parser_state ll1_parser_state(struct ll1_parser *parser) {
    assert(parser != NULL);
    return (struct ll1_parser_state) { .parser = parser };
}

static void push_rule(unsigned r, struct array *syms, unsigned **rtable) {
    unsigned *s = rtable[r];
    while (*s) apush(s, syms), s++;
};

static bool start_scanning(char *input, struct ll1_parser_state *state) {
    if (nfa_start_match(input, &state->match, &state->parser->scanner)) {
        state->lookahead = nfa_match(&state->match);
        debug("initial lookahead: %u\n", state->lookahead);
        return true;
    }

    return false;
}

static bool peek(unsigned expected, struct ll1_parser_state *state) {
    return state->lookahead == expected;
}

static bool expect(unsigned expected, struct ll1_parser_state *state) {
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
    for (int i = 0; i < asize(syms); i++) {
        at(&sym, i, syms);
        debug("%u ", sym);
    }

    debug("\n");
}

#define SYM_STACK_SIZE 7
bool ll1_parse(struct ll1_error *error, char *input, struct ll1_parser_state *state) {
    assert(state != NULL);

    if (!start_scanning(input, state)) return oom_error(error, NULL);

    struct array *syms = init_array(sizeof (unsigned), SYM_STACK_SIZE, 0, 0);
    if (!syms) return oom_error(error, NULL);

    struct ll1_parser const *parser = state->parser;
    unsigned **rtable = parser->rtable,
             **ptable = parser->ptable;

    struct gram_stats stats = parser->stats;
    unsigned const nonterm0 = stats.terms + 1;

    push_rule(GM_START, syms, rtable);

    bool success = true;
    unsigned sym = 0;

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
            unsigned rule = ptable[NTI(sym, stats)][TI(state->lookahead)];
            debug(" = %u\n", rule);

            if (rule) {
                apop(&sym, syms);
                push_rule(rule, syms, rtable);
            } else {
                success = syntax_error(error, sym, state);
            }
        }
    }

    free_array(syms);

    return success;
}

void free_ll1_parser_state(struct ll1_parser_state *state) {
    free_nfa_match(&state->match);
}

#define ERROR_FMT_END "\n|\n"
#define OOM_ERROR_FMT_START "| LL1 Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
#define NOT_LL1_FMT "| Grammar Not LL1\n|\n"
#define SYNTAX_ERROR_FMT_START "| LL1 Syntax Error\n|\n| Got: %u\n| Expected: %u"
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
void print_ll1_error(FILE *handle, struct ll1_error error) {
    switch (error.type) {
        case GM_LL1_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, error.actual, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_LL1_NOT_LL1_ERROR:
            fprintf(handle, NOT_LL1_FMT);
            break;
        case GM_LL1_SCANNER_ERROR:
            print_regex_error(handle, error.scanerr);
            break;
        case GM_LL1_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
    }
}

