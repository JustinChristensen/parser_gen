#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <base/debug.h>
#include <base/string.h>
#include <base/bitset.h>
#include <regex/base.h>
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"

#define debug(...) debug_ns("gram_analyze", __VA_ARGS__);

#define offs(n) ((n) + 1)
#define nullterm(n) ((n) + 1)

void gram_count(struct gram_parser_spec *spec) {
    assert(spec != NULL);
    assert(spec->type == GM_PACKED_SPEC);

    struct gram_stats stats = { 0 };

    if (spec->patterns) {
        struct regex_pattern *pattern = spec->patterns;
        while (!regex_null_pattern(pattern))
            stats.patterns++, pattern++;
    }

    if (spec->symbols) {
        struct gram_symbol *sym = gram_symbol0(spec);
        while (!gram_symbol_null(sym)) {
            if (sym->type == GM_TERM) stats.terms++;
            else stats.nonterms++;
            stats.symbols++;
            sym++;
        }
    }

    if (spec->rules) {
        unsigned **rule = gram_rule0(spec);
        while (*rule) {
            unsigned *s = *rule;
            while (*s) stats.rsymbols++, s++;
            stats.rules++;
            rule++;
        }
    }

    spec->stats = stats;
}

#define STATS_TITLE_FMT "stats:\n"
#define STATS_FMT "  patterns: %d\n  terms: %d\n  nonterms: %d\n  symbols: %d\n  rules: %d\n  rsymbols: %d\n"
void print_gram_stats(FILE *handle, struct gram_stats const stats) {
    fprintf(handle, STATS_TITLE_FMT);
    fprintf(handle, STATS_FMT,
        stats.patterns,
        stats.terms,
        stats.nonterms,
        stats.symbols,
        stats.rules,
        stats.rsymbols);
    fprintf(handle, "\n");
}

static void free_sets(struct bitset **sets, unsigned n) {
    if (!sets) return;

    for (int i = 1; i < n; i++) {
        free(sets[i]);
    }
    free(sets);
}

#define NULLABLE_HEADER_FMT "  %4s  %s\n"
#define NULLABLE_ROW_FMT    "  %4d  %s\n"
static void print_nullable(FILE *handle, bool const *nullable, unsigned n) {
    if (!nullable) return;
    fprintf(handle, NULLABLE_HEADER_FMT, "num", "nullable");
    for (int i = 1; i < n; i++) {
        fprintf(handle, NULLABLE_ROW_FMT, i, yesno(nullable[i]));
    }
    fprintf(handle, "\n");
}

static void print_sets(FILE *handle, struct bitset **sets, unsigned n) {
    if (!sets) return;

    fprintf(handle, "  %4s  %-s\n", "num", "set");
    for (int i = 1; i < n; i++) {
        fprintf(handle, "  %4d  ", i);
        print_bitset(handle, sets[i]);
        fprintf(handle, "\n");
    }
    fprintf(handle, "\n");
}

#define SYMBOL_NULLABLE(name) \
static bool name( \
    bool *added, bool *nullable, unsigned s, struct gram_parser_spec const *spec \
)
#define RULE_NULLABLE(name) \
static bool name( \
    bool *added, bool *nullable, unsigned r, struct gram_parser_spec const *spec \
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
        unsigned *r = sym.derives;
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
    unsigned *s = spec->rules[r];
    bool rnull = true;
    while (*s) {
        bool snull = symbol_nullable(added, nullable, *s, spec);
        rnull = rnull && snull; // a rule is nullable if all of it's symbols are nullable
        s++;
    }
    return rnull;
}

static bool *symbols_nullable(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    bool *nullable = calloc(an->nsymbols, sizeof *nullable);
    if (!nullable) return NULL;

    bool *added = calloc(an->nsymbols, sizeof *added);
    if (!added) return free(nullable), NULL;

    struct gram_symbol *start = gram_nonterm0(spec);
    symbol_nullable(added, nullable, start->num, spec);

    free(added);

    return nullable;
}

struct bitset **alloc_sets(unsigned nsets, unsigned size) {
    struct bitset **sets = calloc(nsets, sizeof *sets);
    if (!sets) return NULL;

    for (int i = 1; i < nsets; i++) {
        if (!(sets[i] = bitset(size)))
            return free_sets(sets, i), NULL;
    }

    return sets;
}

#define RULE_FIRST(name) \
static void name( \
    bool *added, struct bitset *first, struct gram_symbol_analysis *an, \
    unsigned r, struct gram_parser_spec const *spec \
)
#define SYMBOL_FIRST(name) \
static void name( \
    bool *added, struct bitset *first, struct gram_symbol_analysis *an, \
    unsigned s, struct gram_parser_spec const *spec \
)

RULE_FIRST(rule_first);
SYMBOL_FIRST(symbol_first);

SYMBOL_FIRST(symbol_first) {
    invariant(assert_symbol_index, s, spec);

    struct gram_symbol sym = spec->symbols[s];

    if (added[s]) return;
    added[s] = true;

    if (sym.type == GM_TERM) {
        bsins(sym.num, first);
    } else {
        unsigned *r = sym.derives;
        while (*r) rule_first(added, first, an, *r, spec), r++;
    }
}

RULE_FIRST(rule_first) {
    invariant(assert_rule_index, r, spec);

    unsigned *s = spec->rules[r];

    while (*s) {
        symbol_first(added, first, an, *s, spec);
        if (!an->nullable[*s]) break;
        s++;
    }
}

static struct bitset **symbols_firsts(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    unsigned nsymbols = an->nsymbols;
    struct bitset **firsts = alloc_sets(nsymbols, spec->stats.terms);
    if (!firsts) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free_sets(firsts, nsymbols), NULL;

    struct gram_symbol *s = gram_symbol0(spec);
    while (!gram_symbol_null(s)) {
        symbol_first(added, firsts[s->num], an, s->num, spec);
        memset(added, false, nsymbols);
        s++;
    }

    free(added);

    return firsts;
}

#define RULE_FOLLOWS(name) \
static void name( \
    bool *added, struct bitset **follows, struct gram_symbol_analysis *an,  \
    unsigned r, unsigned nt, struct gram_parser_spec const *spec \
)

#define SYMBOL_FOLLOWS(name) \
static void name( \
    bool *added, struct bitset **follows, struct gram_symbol_analysis *an, \
    unsigned s, struct gram_parser_spec const *spec \
)

RULE_FOLLOWS(rule_follows);
SYMBOL_FOLLOWS(_symbol_follows);

SYMBOL_FOLLOWS(_symbol_follows) {
    invariant(assert_symbol_index, s, spec);
    struct gram_symbol sym = spec->symbols[s];

    if (added[sym.num]) return;
    added[sym.num] = true;

    if (sym.type == GM_NONTERM) {
        unsigned *r = sym.derives;
        while (*r) {
            rule_follows(added, follows, an, *r, sym.num, spec);
            r++;
        }
    }
}

RULE_FOLLOWS(rule_follows) {
    invariant(assert_rule_index, r, spec);
    unsigned *s = spec->rules[r];
    while (*s) {
        unsigned *n = s + 1;

        // add the first sets for the symbols up to the first non-nullable symbol
        while (*n) {
            bsunion(follows[*s], an->firsts[*n]);
            if (!an->nullable[*n]) break;
            n++;
        }

        // if we're at the end, add the follow set for the current non-terminal
        if (!*n) bsunion(follows[*s], follows[nt]);

        _symbol_follows(added, follows, an, *s, spec);

        s++;
    }
}

static size_t all_sets_size(struct bitset **follows, struct gram_parser_spec const *spec) {
    size_t size = 0;

    for (int i = 1; i < offs(spec->stats.symbols); i++) {
        size += bssize(follows[i]);
    }

    return size;
}

static size_t
symbol_follows(
    bool *added, struct bitset **follows, struct gram_symbol_analysis *an,
    unsigned s, struct gram_parser_spec const *spec
) {
    _symbol_follows(added, follows, an, s, spec);
    return all_sets_size(follows, spec);
}

static struct bitset **symbols_follows(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    unsigned nsymbols = an->nsymbols;

    struct bitset **follows = alloc_sets(nsymbols, spec->stats.terms);
    if (!follows) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free_sets(follows, nsymbols), NULL;

    // add $ to the follow set for the start symbol
    struct gram_symbol *start = gram_nonterm0(spec);

    bsins(GM_EOF, follows[start->num]);

    // use cardinality of all sets to compute equivalence, and continue until we find the least fixed point
    size_t p = 1, n;
    int passes = 1;
    while ((n = symbol_follows(added, follows, an, start->num, spec)) != p) {
        p = n;
        memset(added, false, nsymbols);
        passes++;
    }

    debug("computing the follow sets required %d total passes\n", passes);

    free(added);

    return follows;
}

bool gram_analyze_symbols(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    free_gram_symbol_analysis(an);

    if (!gram_has_rules(spec)) return true;

    an->nsymbols = offs(spec->stats.symbols);

    an->nullable = symbols_nullable(an, spec);
    if (!an->nullable) return false;

    an->firsts = symbols_firsts(an, spec);
    if (!an->firsts) return free_gram_symbol_analysis(an), false;

    an->follows = symbols_follows(an, spec);
    if (!an->follows) return free_gram_symbol_analysis(an), false;

    return true;
}

void free_gram_symbol_analysis(struct gram_symbol_analysis *an) {
    if (!an) return;

    if (an->nullable) free(an->nullable);
    if (an->firsts) free_sets(an->firsts, an->nsymbols);
    if (an->follows) free_sets(an->follows, an->nsymbols);
    *an = (struct gram_symbol_analysis) { 0 };
}

void print_gram_symbol_analysis(FILE *handle, struct gram_symbol_analysis const *an) {
    assert(an != NULL);

    fprintf(handle, "symbol nullable:\n");
    print_nullable(handle, an->nullable, an->nsymbols);

    fprintf(handle, "symbol firsts:\n");
    print_sets(handle, an->firsts, an->nsymbols);

    fprintf(handle, "symbol follows:\n");
    print_sets(handle, an->follows, an->nsymbols);
}

static bool *rules_nullable(
    struct gram_rule_analysis *an, struct gram_symbol_analysis *syman,
    struct gram_parser_spec const *spec
) {
    bool *rnullable = calloc(an->nrules, sizeof *rnullable);
    if (!rnullable) return NULL;

    bool *nullable = syman->nullable;

    for (int i = 1; i < an->nrules; i++) {
        unsigned *s = spec->rules[i];

        bool rnull = true;
        while (*s && (rnull = rnull && nullable[*s])) s++;
        rnullable[i] = rnull;
    }

    return rnullable;
}

static struct bitset **rules_ffollows(
    struct gram_rule_analysis *an, struct gram_symbol_analysis *syman,
    struct gram_parser_spec const *spec
) {
    struct bitset **ffollows = alloc_sets(an->nrules, spec->stats.terms);
    if (!ffollows) return NULL;

    struct gram_symbol *nt = gram_nonterm0(spec);
    while (!gram_symbol_null(nt)) {
        unsigned *r = nt->derives;

        while (*r) {
            unsigned *s = spec->rules[*r];

            // skip the empty rules
            if (!*s) {
                r++; continue;
            }

            while (*s) {
                bsunion(ffollows[*r], syman->firsts[*s]);
                if (!syman->nullable[*s]) break;
                s++;
            }

            if (!*s) bsunion(ffollows[*r], syman->follows[nt->num]);

            r++;
        }

        nt++;
    }

    return ffollows;
}

bool gram_analyze_rules(struct gram_rule_analysis *an, struct gram_symbol_analysis *syman, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    free_gram_rule_analysis(an);

    if (!gram_has_rules(spec)) return true;

    an->nrules = offs(spec->stats.rules);

    an->nullable = rules_nullable(an, syman, spec);
    if (!an->nullable) return false;

    an->ffollows = rules_ffollows(an, syman, spec);
    if (!an->ffollows) return false;

    return true;
}

void free_gram_rule_analysis(struct gram_rule_analysis *an) {
    if (!an) return;

    if (an->nullable) free(an->nullable);
    if (an->ffollows) free_sets(an->ffollows, an->nrules);
}

void print_gram_rule_analysis(FILE *handle, struct gram_rule_analysis const *an) {
    assert(an != NULL);

    fprintf(handle, "rule nullable:\n");
    print_nullable(handle, an->nullable, an->nrules);

    fprintf(handle, "rule first/follows:\n");
    print_sets(handle, an->ffollows, an->nrules);
}

// bool gram_is_ll1(
//     struct ll1_check_result *result,
//     bool const *nullable, struct bitset **firsts, struct bitset **follows,
//     struct gram_parser_spec const *spec
// ) {
//     invariant(assert_packed_spec, spec);
//     assert(result != NULL);
//     assert(nullable != NULL);
//     assert(firsts != NULL);
//     assert(follows != NULL);
//
//     *result = NULL;
//
//     if (!gram_has_rules(spec)) return true;
//
//     struct gram_stats const stats = spec->stats;
//
//     bool *visited = calloc(offs(stats.symbols), sizeof *visited);
//
//     struct gram_symbol *start = gram_nonterm0(spec);
//
//
//
//     free(visited);
//
//     return true;
// }

