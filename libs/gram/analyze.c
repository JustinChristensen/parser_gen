#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <base/debug.h>
#include <base/intset.h>
#include "gram/spec.h"
#include "gram/analyze.h"

static bool _gram_first(bool *added, struct intset **set, unsigned int i, struct gram_parser_spec *spec) {
    invariants(assert_gram_sym_index, i, spec->stats);

    struct gram_symbol s = spec->symbols[i];

    if (added[i]) return s.nullable;
    added[i] = true;

    if (s.type == GM_TERM) {
        *set = sinsert(s.num, *set);
    } else {
        unsigned int *rn = s.rules;
        while (*rn) {
            unsigned int *r = spec->rules[*rn];
            if (*r)
                while (_gram_first(added, set, *r, spec))
                    r++;
            rn++;
        }
    }

    return s.nullable;
}

bool gram_first(struct intset **set, unsigned int i, struct gram_parser_spec *spec) {
    bool *added = calloc(spec->stats.symbols + 1, sizeof *added);
    bool nullable = _gram_first(added, set, i, spec);
    free(added);
    return nullable;
}

// TODO: wrap this in a struct (maybe with a VLA)
struct intset **gram_firsts(struct gram_parser_spec *spec) {
    unsigned int nsyms = spec->stats.symbols + 1;
    struct intset **sets = calloc(spec->stats.symbols, sizeof *sets), **set = sets;
    bool *added = calloc(nsyms, sizeof *added);

    struct gram_symbol *sym = &spec->symbols[1];
    while (sym->num) {
        _gram_first(added, set++, sym->num, spec);
        memset(added, false, nsyms);
        sym++;
    }

    free(added);

    return sets;
}

void free_gram_firsts(struct intset **sets, struct gram_parser_spec *spec) {
    for (unsigned int i = 0; i < spec->stats.symbols; i++) {
        free_intset(sets[i]);
        sets[i] = NULL;
    }
    free(sets);
}

void print_gram_firsts(FILE *handle, struct intset **sets, struct gram_parser_spec *spec) {
    fprintf(handle, "first sets:\n");
    fprintf(handle, "  %4s  %-s\n", "num", "set");
    for (unsigned int i = 0; i < spec->stats.symbols; i++) {
        fprintf(handle, "  %4d  ", i + 1);
        print_intset(sets[i]);
    }
    fprintf(handle, "\n");
}

