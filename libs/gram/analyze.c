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

bool *
gram_nullable(struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    if (!gram_has_rules(spec)) return NULL;

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
    for (int i = 1; i < offs(spec->stats.symbols); i++) {
        fprintf(handle, NULLABLE_ROW_FMT, i, yesno(nullable[i]));
    }
    fprintf(handle, "\n");
}

struct bitset **alloc_sets(struct gram_stats const stats) {
    struct bitset **sets = calloc(offs(stats.symbols), sizeof *sets);
    if (!sets) return NULL;

    for (int i = 1; i < offs(stats.symbols); i++) {
        if (!(sets[i] = bitset(stats.terms))) {
            free_gram_sets(sets, stats);
            return NULL;
        }
    }

    return sets;
}

void free_gram_sets(struct bitset **sets, struct gram_stats const stats) {
    if (!sets) return;

    for (int i = 1; i < offs(stats.symbols); i++) {
        free(sets[i]);
    }

    free(sets);
}

void print_gram_sets(FILE *handle, struct bitset **sets, struct gram_stats const stats) {
    if (!sets) return;

    fprintf(handle, "  %4s  %-s\n", "num", "set");
    for (int i = 1; i < offs(stats.symbols); i++) {
        fprintf(handle, "  %4d  ", i);
        print_bitset(handle, sets[i]);
        fprintf(handle, "\n");
    }
    fprintf(handle, "\n");
}


#define RULE_FIRST(name) \
static void name( \
    bool *added, struct bitset **first, bool const *nullable, \
    unsigned r, struct gram_parser_spec const *spec \
)
#define SYMBOL_FIRST(name) \
static void name( \
    bool *added, struct bitset **first, bool const *nullable, \
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
        bsins(sym.num, *first);
    } else {
        unsigned *r = sym.derives;
        while (*r) rule_first(added, first, nullable, *r, spec), r++;
    }
}

RULE_FIRST(rule_first) {
    invariant(assert_rule_index, r, spec);

    unsigned *s = spec->rules[r];
    while (*s) {
        symbol_first(added, first, nullable, *s, spec);
        if (!nullable[*s]) break;
        s++;
    }
}

// FIXME: this doesn't take into account the reachability of symbols from the start rule
struct bitset **gram_firsts(bool const *nullable, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    if (!gram_has_rules(spec)) return NULL;

    struct gram_stats stats = spec->stats;

    struct bitset **firsts = alloc_sets(stats);
    if (!firsts) return NULL;

    bool *added = calloc(offs(stats.symbols), sizeof *added);
    if (!added) return free_gram_sets(firsts, stats), NULL;

    struct gram_symbol *s = gram_symbol0(spec);
    while (!gram_symbol_null(s)) {
        symbol_first(added, &firsts[s->num], nullable, s->num, spec);
        memset(added, false, offs(stats.symbols));
        s++;
    }

    free(added);

    return firsts;
}

#define RULE_FOLLOWS(name) \
static void name( \
    bool *added, bool const *nullable, struct bitset **firsts, struct bitset **follows, \
    unsigned r, unsigned nt, struct gram_parser_spec const *spec \
)

#define SYMBOL_FOLLOWS(name) \
static void name( \
    bool *added, bool const *nullable, struct bitset **firsts, struct bitset **follows, \
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
            rule_follows(added, nullable, firsts, follows, *r, sym.num, spec);
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
            bsunion(follows[*s], firsts[*n]);
            if (!nullable[*n]) break;
            n++;
        }

        // if we're at the end, add the follow set for the current non-terminal
        if (!*n) bsunion(follows[*s], follows[nt]);

        _symbol_follows(added, nullable, firsts, follows, *s, spec);

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
    bool *added, bool const *nullable, struct bitset **firsts,
    struct bitset **follows, unsigned s, struct gram_parser_spec const *spec
) {
    _symbol_follows(added, nullable, firsts, follows, s, spec);
    return all_sets_size(follows, spec);
}

struct bitset **gram_follows(bool const *nullable, struct bitset **firsts, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    if (!gram_has_rules(spec)) return NULL;

    struct gram_stats stats = spec->stats;

    struct bitset **follows = alloc_sets(stats);
    if (!follows) return NULL;

    bool *added = calloc(offs(stats.symbols), sizeof *added);
    if (!added) return free_gram_sets(follows, stats), NULL;

    // add $ to the follow set for the start symbol
    struct gram_symbol *start = gram_nonterm0(spec);
    bsins(GM_EOF, follows[start->num]);

    // use cardinality of all sets to compute equivalence, and continue until we find the least fixed point
    size_t p = 1, n;
    int i = 1;
    while ((n = symbol_follows(added, nullable, firsts, follows, start->num, spec)) != p) {
        p = n;
        memset(added, false, offs(stats.symbols));
        i++;
    }
    debug("computing the follow sets required %d total passes\n", i);

    free(added);

    return follows;
}

bool gram_is_ll1(
    struct ll1_error *error,
    bool const *nullable, struct bitset **firsts, struct bitset **follows,
    struct gram_parser_spec const *spec
) {
    return true;
}

#define OOM_ERROR_FMT_START "| Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"

void print_ll1_error(FILE *handle, struct ll1_error error) {
    switch (error.type) {
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

