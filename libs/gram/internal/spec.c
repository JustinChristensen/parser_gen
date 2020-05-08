#ifndef GRAM_SPEC_C_
#define GRAM_SPEC_C_ 1

#include <stdlib.h>
#include <regex/base.h>
#include "gram/parser.h"
#include "gram/spec.h"

#include "macros.c"

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

#define FOR_SYMBOL(stats, s) \
    for (gram_sym_no s = GM_SYMBOL0; s < offs((stats).symbols); s++)

#define FOR_RULE(stats, r) \
    for (gram_rule_no r = GM_START; r < offs((stats).rules); r++)

#define NONTERM0(stats) (offs((stats).terms))

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
        if (sym->str) free(sym->str);
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

static unsigned rulesize(gram_sym_no *s) {
    unsigned size = 0;
    while (*s++) size++;
    return size;
}

#pragma clang diagnostic pop

#endif // GRAM_SPEC_C_
