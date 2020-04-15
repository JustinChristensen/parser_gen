#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/base.h>
#include <base/assert.h>
#include <base/array.h>
#include <base/intset.h>
#include "gram/ll1.h"
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"

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

struct ll1_parser ll1_parser(struct nfa_context scanner, unsigned **ptable, unsigned **rtable) {
    return (struct ll1_parser) { ptable, rtable, scanner };
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

#define PTABLE_STACK 7
static unsigned **parse_table(
    bool const *nullable, struct intset **firsts, struct intset **follows,
    struct gram_parser_spec const *spec
) {
    struct gram_stats stats = spec->stats;

    unsigned **ptable = alloc_parse_table(stats);
    if (!ptable) return NULL;

    struct intset_iterator it = { 0 };
    siterator(NULL, &it);

    struct gram_symbol *sym = gram_nonterm0(spec);
    while (!gram_symbol_null(sym)) {
        unsigned nt = sym->num;
        unsigned *r = sym->derives;

        while (*r) {
            unsigned *s = spec->rules[*r];

            // ptable[nonterm][term] = rule
            while (*s) {
                reset_siterator(firsts[*s], &it);
                int t;
                while (snext(&t, &it)) ptable[nt][(unsigned) t] = *r;
                if (!nullable[*s]) break;
                s++;
            }

            if (!*s) {
                reset_siterator(follows[nt], &it);
                int t;
                while (snext(&t, &it)) ptable[nt][(unsigned) t] = *r;
            }

            r++;
        }

        sym++;
    }

    free_siterator(&it);

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
    unsigned **rtable = alloc_rule_table(spec->stats);
    if (!rtable) return NULL;

    // offset the rule table by 1
    rtable[0] = NULL;

    // start counting 1 past
    unsigned **trule = &rtable[1];
    // symbol lists start after the list pointers
    unsigned *tsym = (unsigned *) (rtable + nullterm(offs(spec->stats.rules)));

    unsigned **r = gram_rule0(spec);
    while (*r) {
        *trule = tsym;
        unsigned *s = r[rsize(*r)];

        while (s != *r) *tsym++ = *--s;
        *tsym++ = 0;

        trule++, r++;
    }

    // null-terminate
    *trule = NULL;

    return rtable;
}

bool gen_ll1(struct ll1_error *error, struct ll1_parser *parser, struct gram_parser_spec *spec) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);

    prod(error, ((struct ll1_error) { 0 }));
    *parser = (struct ll1_parser) { 0 };

    bool *nullable = NULL;
    struct intset **firsts = NULL, **follows = NULL;
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

    if (!nfa_context(&scanner, spec->patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    ptable = parse_table(nullable, firsts, follows, spec);
    if (!ptable) {
        oom_error(error, NULL);
        goto free;
    }

    rtable = rule_table(spec);
    if (!rtable) {
        oom_error(error, NULL);
        goto free;
    }

    *parser = ll1_parser(scanner, ptable, rtable);

    return true;
free:
    free(nullable);
    free_gram_sets(firsts, spec);
    free_gram_sets(follows, spec);
    free_nfa_context(&scanner);
    free(ptable);
    free(rtable);

    return false;
}

