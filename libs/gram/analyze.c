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

// static bool rule_first(bool *added, struct intset **set, unsigned int r, struct gram_parser_spec *spec);
// static bool symbol_first(bool *added, struct intset **set, unsigned int i, struct gram_parser_spec *spec);
//
// static bool symbol_first(bool *added, struct intset **set, unsigned int i, struct gram_parser_spec *spec) {
//     invariant(assert_symbol_index, i, spec);
//
//     struct gram_symbol s = spec->symbols[i];
//
//     if (added[i]) return s.nullable;
//     added[i] = true;
//
//     if (s.type == GM_TERM) {
//         *set = sinsert(s.num, *set);
//     } else {
//         unsigned int *d = s.derives;
//         while (*d) rule_first(added, set, *d, spec), d++;
//     }
//
//     return s.nullable;
// }
//
// static bool rule_first(bool *added, struct intset **set, unsigned int i, struct gram_parser_spec *spec) {
//     invariant(assert_rule_index, i, spec);
//     unsigned int *r = spec->rules[i];
//     while (*r && symbol_first(added, set, *r, spec)) r++;
// }
//
// static struct gram_sets *alloc_sets(unsigned int symbols) {
//     int nsets = symbols + 1;
//     size_t ssize = sizeof (struct intset *) * nsets;
//     struct gram_sets *sets = malloc(sizeof *sets + ssize);
//     if (!sets) return NULL;
//     sets->n = nsets;
//     memset(sets->s, 0, ssize);
//     return sets;
// }
//
// struct gram_sets *gram_firsts(struct gram_parser_spec *spec) {
//     struct gram_sets *firsts = alloc_sets(spec->stats.symbols);
//     if (!firsts) return NULL;
//
//     unsigned int nsymbols = spec->stats.symbols + 1;
//     bool *added = calloc(nsymbols, sizeof *added);
//     if (!added) return free(firsts), NULL;
//
//     struct intset **sets = firsts->sets;
//
//     struct gram_symbol *sym = gram_symbol0(spec);
//     while (!gram_null_symbol(sym)) {
//         gram_sym_first(added, &sets[sym->num], sym->num, spec);
//         memset(added, false, nsymbols);
//         sym++;
//     }
//
//     free(added);
//
//     return firsts;
// }

static bool symbol_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec *spec);
static bool rule_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec *spec);

static bool
symbol_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec *spec) {
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
rule_nullable(bool *added, bool *nullable, unsigned int i, struct gram_parser_spec *spec) {
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
gram_nullable(struct gram_parser_spec *spec) {
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
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_stats stats) {
    fprintf(handle, NULLABLE_TITLE_FMT);
    fprintf(handle, NULLABLE_HEADER_FMT, "num", "nullable");
    for (int i = 1; i < stats.symbols + 1; i++) {
        fprintf(handle, NULLABLE_ROW_FMT, i, yesno(nullable[i]));
    }
    fprintf(handle, "\n");
}

// void free_gram_sets(struct gram_sets *sets) {
//     assert(sets != NULL);
//
//     for (int i = 1; i < sets->n; i++) {
//         free_intset(sets->s[i]);
//         sets->s[i] = NULL;
//     }
//     free(sets);
// }

// void print_gram_sets(FILE *handle, struct gram_sets const *sets) {
//     assert(sets != NULL);
//
//     fprintf(handle, "  %4s  %-s\n", "num", "set");
//     for (int i = 1; i < sets->n; i++) {
//         fprintf(handle, "  %4d  ", i);
//         print_intset(sets->s[i]);
//     }
//     fprintf(handle, "\n");
// }

