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

static void rule_first(bool *added, struct intset **set, unsigned int r, bool const *nullable, struct gram_parser_spec const *spec);
static void symbol_first(bool *added, struct intset **set, unsigned int i, bool const *nullable, struct gram_parser_spec const *spec);

static void symbol_first(bool *added, struct intset **set, unsigned int i, bool const *nullable, struct gram_parser_spec const *spec) {
    invariant(assert_symbol_index, i, spec);

    struct gram_symbol s = spec->symbols[i];

    if (added[i]) return;
    added[i] = true;

    if (s.type == GM_TERM) {
        *set = sinsert(s.num, *set);
    } else {
        unsigned int *d = s.derives;
        while (*d) rule_first(added, set, *d, nullable, spec), d++;
    }
    debug("computed symbol %d first set\n", i);
}

static void rule_first(bool *added, struct intset **set, unsigned int i, bool const *nullable, struct gram_parser_spec const *spec) {
    invariant(assert_rule_index, i, spec);
    unsigned int *r = spec->rules[i];
    while (*r) {
        symbol_first(added, set, *r, nullable, spec);
        if (!nullable[*r]) break;
        r++;
    }
    debug("computed rule %d first set\n", i);
}

struct intset **gram_firsts(bool const *nullable, struct gram_parser_spec const *spec) {
    int nsymbols = spec->stats.symbols + 1;
    struct intset **firsts = calloc(nsymbols, sizeof *firsts);
    if (!firsts) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free(firsts), NULL;

    if (gram_has_rules(spec)) {
        struct gram_symbol *s = gram_symbol0(spec);
        while (!gram_null_symbol(s)) {
            symbol_first(added, &firsts[s->num], s->num, nullable, spec);
            memset(added, false, nsymbols);
            s++;
        }
    }

    free(added);

    return firsts;
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

static bool symbol_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec const *spec);
static bool rule_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec const *spec);

static bool
symbol_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec const *spec) {
    invariant(assert_symbol_index, i, spec);
    struct gram_symbol s = spec->symbols[i];

    if (added[s.num]) return nullable[s.num];
    added[s.num] = true;

    bool snull = false;

    if (s.type == GM_NONTERM) {
        unsigned int *d = s.derives;
        while (*d) {
            bool rnull = rule_nullable(added, nullable, *d, spec);
            snull = snull || rnull;
            d++;
        }
    }

    nullable[s.num] = snull;

    debug("symbol %d nullable: %s\n", i, yesno(snull));
    return snull;
}

static bool
rule_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec const *spec) {
    invariant(assert_rule_index, i, spec);
    unsigned int *r = spec->rules[i];
    bool rnull = true;
    while (*r) {
        bool snull = symbol_nullable(added, nullable, *r, spec);
        rnull = rnull && snull;
        r++;
    }
    debug("rule %d nullable: %s\n", i, yesno(rnull));
    return rnull;
}

bool *
gram_nullable(struct gram_parser_spec const *spec) {
    int nsyms = spec->stats.symbols + 1;
    bool *nullable = calloc(nsyms, sizeof *nullable);
    if (!nullable) return NULL;

    bool *added = calloc(nsyms, sizeof *added);
    if (!added) return free(nullable), NULL;

    if (gram_has_rules(spec)) {
        struct gram_symbol *start = gram_nonterm0(spec);
        symbol_nullable(added, nullable, start->num, spec);
    }

    free(added);

    return nullable;
}

#define NULLABLE_TITLE_FMT  "nullable:\n"
#define NULLABLE_HEADER_FMT "  %4s  %s\n"
#define NULLABLE_ROW_FMT    "  %4d  %s\n"
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_parser_spec const *spec) {
    fprintf(handle, NULLABLE_TITLE_FMT);
    fprintf(handle, NULLABLE_HEADER_FMT, "num", "nullable");
    for (int i = 1; i < spec->stats.symbols + 1; i++) {
        fprintf(handle, NULLABLE_ROW_FMT, i, yesno(nullable[i]));
    }
    fprintf(handle, "\n");
}

