#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/base.h>
#include <base/assert.h>
#include <base/debug.h>
#include <base/bitset.h>
#include "gram/ll1.h"
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"

#define debug(...) debug_ns("gram_ll1", __VA_ARGS__);

static bool
_oom_error(struct ll1_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct ll1_error) { .type = GM_LL1_OOM_ERROR, .file = file, .col = col }));

    return false;
}

static bool
scanner_error(struct ll1_error *error, struct regex_error scanerr) {
    prod(error, ((struct ll1_error) { .type = GM_LL1_SCANNER_ERROR, .scanerr = scanerr }));
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
static unsigned **parse_table(
    bool const *nullable, struct bitset **firsts, struct bitset **follows,
    struct gram_parser_spec const *spec
) {
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
                struct bsiter it = bsiter(firsts[*s]);
                unsigned t;
                while (bsnext(&t, &it)) {
                    unsigned ti = TI(t);
                    debug("ptable[%u][%u] = %u\n", nti, ti, *r);
                    ptable[nti][ti] = *r;
                }
                if (!nullable[*s]) break;
                s++;
            }

            if (!*s) {
                struct bsiter it = bsiter(follows[nt]);
                unsigned t;
                while (bsnext(&t, &it)) {
                    unsigned ti = TI(t);
                    debug("ptable[%u][%u] = %u\n", nti, ti, *r);
                    ptable[nti][ti] = *r;
                }
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

    bool *nullable = NULL;
    struct bitset **firsts = NULL, **follows = NULL;
    struct nfa_context scanner = { 0 };
    unsigned **ptable = NULL, **rtable = NULL;

    nullable = gram_nullable(spec);
    if (!nullable) return oom_error(error, NULL);

    firsts = gram_firsts(nullable, spec);
    if (!firsts) return oom_error(error, nullable);

    follows = gram_follows(nullable, firsts, spec);
    if (!follows) {
        oom_error(error, NULL);
        goto free;
    }

    if (!gram_is_ll1(error, nullable, firsts, follows, spec))
        goto free;

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

    ptable = parse_table(nullable, firsts, follows, spec);
    if (!ptable) {
        oom_error(error, NULL);
        goto free;
    }

    *parser = ll1_parser(scanner, rtable, ptable, stats);

    free(nullable);
    free_gram_sets(firsts, stats);
    free_gram_sets(follows, stats);

    return true;
free:
    free(nullable);
    free_gram_sets(firsts, stats);
    free_gram_sets(follows, stats);
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

