#ifndef GRAM_SPEC_C
#define GRAM_SPEC_C 1

#include <stdlib.h>
#include <regex/base.h>
#include "gram/parser.h"

static void free_patterns(struct regex_pattern *patterns) {
    if (!patterns) return;
    struct regex_pattern *p = patterns;
    while (p->sym) {
        free(p->tag);
        free(p->pattern);
        p++;
    }
    free(patterns);
}

static void free_symbols(struct gram_symbol *symbols) {
    if (!symbols) return;

    struct gram_symbol *sym = &symbols[1];
    while (sym->num) {
        if (sym->derives) free(sym->derives);
        sym++;
    }

    free(symbols);
}

static void free_rules(gram_sym_no **rules) {
    if (!rules) return;
    gram_sym_no **r = &rules[1];
    while (*r) free(*r), r++;
    free(rules);
}

#endif // GRAM_SPEC_C
