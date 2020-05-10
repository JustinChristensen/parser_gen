#ifndef GRAM_SPEC_C_
#define GRAM_SPEC_C_ 1

#include <stdlib.h>
#include <regex/base.h>
#include "gram/parser.h"
#include "gram/spec.h"

#include "macros.c"

#define FOR_SYMBOL(stats, s) \
    for (gram_sym_no s = GM_SYMBOL0; s < offs((stats).symbols); s++)

#define FOR_TERM(stats, s) \
    for (gram_sym_no s = GM_SYMBOL0; s < offs((stats).terms); s++)

#define FOR_RULE(stats, r) \
    for (gram_rule_no r = GM_START; r < offs((stats).rules); r++)

#define NONTERM0(stats) (offs((stats).terms))

__attribute__((unused))
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

__attribute__((unused))
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

__attribute__((unused))
static void free_rules(gram_sym_no **rules) {
    if (!rules) return;
    gram_sym_no **r = &rules[1];
    while (*r) free(*r), r++;
    free(rules);
}

__attribute__((unused))
static void free_symtab(char **symtab, struct gram_stats const stats) {
    if (!symtab) return;
    FOR_SYMBOL(stats, s) if (symtab[s]) free(symtab[s]);
    free(symtab);
}

__attribute__((unused))
static unsigned rulesize(gram_sym_no *s) {
    unsigned size = 0;
    while (*s++) size++;
    return size;
}

#endif // GRAM_SPEC_C_
