#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <base/debug.h>
#include <base/string.h>
#include <base/intset.h>
#include "gram/spec.h"
#include "gram/analyze.h"

#include "assert.c"

#define debug(...) debug_ns("gram_analyze", __VA_ARGS__);

#define STATS_TITLE_FMT "stats:\n"
#define STATS_FMT "  patterns: %d\n  terms: %d\n  nonterms: %d\n  symbols: %d\n  rules: %d\n"
void print_gram_stats(FILE *handle, struct gram_stats const stats) {
    fprintf(handle, STATS_TITLE_FMT);
    fprintf(handle, STATS_FMT, stats.patterns, stats.terms, stats.nonterms,
        stats.symbols, stats.rules);
    fprintf(handle, "\n");
}

#define SYMBOL_NULLABLE(name) \
static bool name( \
    bool *added, bool *nullable, unsigned int s, struct gram_parser_spec const *spec \
)
#define RULE_NULLABLE(name) \
static bool name( \
    bool *added, bool *nullable, unsigned int r, struct gram_parser_spec const *spec \
)

SYMBOL_NULLABLE(symbol_nullable);
RULE_NULLABLE(rule_nullable);

SYMBOL_NULLABLE(symbol_nullable) {
    invariant(assert_symbol_index, s, spec);
    struct gram_symbol sym = spec->symbols[s];

    if (added[sym.num]) return nullable[sym.num];
    added[sym.num] = true;

    bool snull = false;

    if (sym.type == GM_NONTERM) {
        unsigned int *r = sym.derives;
        while (*r) {
            bool rnull = rule_nullable(added, nullable, *r, spec);
            snull = snull || rnull; // a non-terminal is nullable if any of it's rules are nullable
            r++;
        }
    }

    nullable[sym.num] = snull;

    return snull;
}

RULE_NULLABLE(rule_nullable) {
    invariant(assert_rule_index, r, spec);
    unsigned int *s = spec->rules[r];
    bool rnull = true;
    while (*s) {
        bool snull = symbol_nullable(added, nullable, *s, spec);
        rnull = rnull && snull; // a rule is nullable if all of it's symbols are nullable
        s++;
    }
    return rnull;
}

bool *
gram_nullable(struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    if (!spec->stats.nonterms) return NULL;

    int nsyms = spec->stats.symbols + 1;
    bool *nullable = calloc(nsyms, sizeof *nullable);
    if (!nullable) return NULL;

    bool *added = calloc(nsyms, sizeof *added);
    if (!added) return free(nullable), NULL;

    struct gram_symbol *start = gram_nonterm0(spec);
    symbol_nullable(added, nullable, start->num, spec);

    free(added);

    return nullable;
}

#define NULLABLE_TITLE_FMT  "nullable:\n"
#define NULLABLE_HEADER_FMT "  %4s  %s\n"
#define NULLABLE_ROW_FMT    "  %4d  %s\n"
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_parser_spec const *spec) {
    if (!nullable) return;
    fprintf(handle, NULLABLE_TITLE_FMT);
    fprintf(handle, NULLABLE_HEADER_FMT, "num", "nullable");
    for (int i = 1; i < spec->stats.symbols + 1; i++) {
        fprintf(handle, NULLABLE_ROW_FMT, i, yesno(nullable[i]));
    }
    fprintf(handle, "\n");
}

#define RULE_FIRST(name) \
static void name( \
    bool *added, struct intset **first, bool const *nullable, \
    unsigned int r, struct gram_parser_spec const *spec \
)
#define SYMBOL_FIRST(name) \
static void name( \
    bool *added, struct intset **first, bool const *nullable, \
    unsigned int s, struct gram_parser_spec const *spec \
)

RULE_FIRST(rule_first);
SYMBOL_FIRST(symbol_first);

SYMBOL_FIRST(symbol_first) {
    invariant(assert_symbol_index, s, spec);

    struct gram_symbol sym = spec->symbols[s];

    if (added[s]) return;
    added[s] = true;

    if (sym.type == GM_TERM) {
        *first = sinsert(sym.num, *first);
    } else {
        unsigned int *r = sym.derives;
        while (*r) rule_first(added, first, nullable, *r, spec), r++;
    }
}

RULE_FIRST(rule_first) {
    invariant(assert_rule_index, r, spec);

    unsigned int *s = spec->rules[r];
    while (*s) {
        symbol_first(added, first, nullable, *s, spec);
        if (!nullable[*s]) break;
        s++;
    }
}

// FIXME: this doesn't take into account the reachability of symbols from the start rule
struct intset **gram_firsts(bool const *nullable, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    if (!spec->stats.nonterms) return NULL;

    int nsymbols = spec->stats.symbols + 1;
    struct intset **firsts = calloc(nsymbols, sizeof *firsts);
    if (!firsts) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free(firsts), NULL;

    struct gram_symbol *s = gram_symbol0(spec);
    while (!gram_symbol_null(s)) {
        symbol_first(added, &firsts[s->num], nullable, s->num, spec);
        memset(added, false, nsymbols);
        s++;
    }

    free(added);

    return firsts;
}

// FIXME: add version of sunion that mutates s and avoids the need for freeing
static void addset(struct intset **follows, unsigned int s, struct intset const *t) {
    struct intset *sf = follows[s];
    follows[s] = sunion(sf, t);
    free_intset(sf);
}

#define RULE_FOLLOWS(name) \
static void name( \
    bool *added, bool const *nullable, struct intset **firsts, struct intset **follows, \
    unsigned int r, unsigned int nt, struct gram_parser_spec const *spec \
)

#define SYMBOL_FOLLOWS(name) \
static void name( \
    bool *added, bool const *nullable, struct intset **firsts, struct intset **follows, \
    unsigned int s, struct gram_parser_spec const *spec \
)

RULE_FOLLOWS(rule_follows);
SYMBOL_FOLLOWS(_symbol_follows);

SYMBOL_FOLLOWS(_symbol_follows) {
    invariant(assert_symbol_index, s, spec);
    struct gram_symbol sym = spec->symbols[s];

    if (added[sym.num]) return;
    added[sym.num] = true;

    if (sym.type == GM_NONTERM) {
        unsigned int *r = sym.derives;
        while (*r) {
            rule_follows(added, nullable, firsts, follows, *r, sym.num, spec);
            r++;
        }
    }
}

RULE_FOLLOWS(rule_follows) {
    invariant(assert_rule_index, r, spec);
    unsigned int *s = spec->rules[r];
    while (*s) {
        unsigned int *n = s + 1;

        // add the first sets for the symbols up to the first non-nullable symbol
        while (*n) {
            addset(follows, *s, firsts[*n]);
            if (!nullable[*n]) break;
            n++;
        }

        // if we're at the end, add the follow set for the current non-terminal
        if (!*n) addset(follows, *s, follows[nt]);

        _symbol_follows(added, nullable, firsts, follows, *s, spec);

        s++;
    }
}

static size_t all_sets_size(struct intset **follows, struct gram_parser_spec const *spec) {
    size_t size = 0;

    for (int i = 1; i < spec->stats.symbols + 1; i++) {
        size += ssize(follows[i]);
    }

    return size;
}

static size_t
symbol_follows(
    bool *added, bool const *nullable, struct intset **firsts,
    struct intset **follows, unsigned int s, struct gram_parser_spec const *spec
) {
    _symbol_follows(added, nullable, firsts, follows, s, spec);
    return all_sets_size(follows, spec);
}

struct intset **gram_follows(bool const *nullable, struct intset **firsts, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    // TODO: better pack implementation
    if (!spec->stats.nonterms) return NULL;

    int nsymbols = spec->stats.symbols + 1;
    struct intset **follows = calloc(nsymbols, sizeof *follows);
    if (!follows) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free(follows), NULL;

    // add $ to the follow set for the start symbol
    struct gram_symbol *start = gram_nonterm0(spec);
    follows[start->num] = sinsert(GM_EOF, follows[start->num]);

    // use cardinality of all sets to compute equivalence, and continue until we find the least fixed point
    size_t p = 1, n;
    int i = 1;
    while ((n = symbol_follows(added, nullable, firsts, follows, start->num, spec)) != p) p = n, i++;
    debug("computing the follow sets required %d total passes\n", i);

    free(added);

    return follows;
}

void free_gram_sets(struct intset **sets, struct gram_parser_spec const *spec) {
    if (!sets) return;

    for (int i = 1; i < spec->stats.symbols + 1; i++) {
        free_intset(sets[i]);
    }

    free(sets);
}

void print_gram_sets(FILE *handle, struct intset **sets, struct gram_parser_spec const *spec) {
    if (!sets) return;

    fprintf(handle, "  %4s  %-s\n", "num", "set");
    for (int i = 1; i < spec->stats.symbols + 1; i++) {
        fprintf(handle, "  %4d  ", i);
        print_intset(handle, sets[i]);
        fprintf(handle, "\n");
    }
    fprintf(handle, "\n");
}
