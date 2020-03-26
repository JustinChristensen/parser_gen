#include <stdlib.h>
#include <stdio.h>
#include <base/hash_table.h>
#include <regex/nfa.h>
#include "gram/ast.h"
#include "gram/pack.h"

static int detnum(struct gram_symbol *sym, struct gram_stats stats) {
    int i = sym->num + 1; // #0 reserved

    if (sym->type == GM_NONTERM)
        i += stats.terms;

    return i;
}

static bool allocate_pattern(struct regex_pattern *pat, int sym, char *tag, char *pattern) {
    if (tag && ((tag = strdup(tag)) == NULL)) return false;
    if (pattern && ((pattern = strdup(pattern)) == NULL)) {
        free(tag);
        return false;
    }

    *pat = regex_pattern(sym, tag, pattern);

    return true;
}

static bool is_empty_rhs(struct gram_rhs *rhs) {
    return rhs == NULL || (rhs->type == GM_EMPTY_RHS && rhs->next == NULL);
}

static struct gram_packed_spec *gram_packed_spec(struct gram_stats stats) {
    // terminated by a null pattern
    struct regex_pattern *patterns = calloc(stats.patterns + 1, sizeof *patterns);
    if (!patterns) return NULL;

    // terminated by a null symbol, indexed by symbol number, and symbol #0 reserved
    struct gram_symbol *symbols = calloc(stats.terms + stats.nonterms + 2, sizeof (*symbols));
    if (!symbols) {
        free(patterns);
        return NULL;
    }

    // terminated by NULL, indexed by rule number
    int **rules = calloc(stats.rules + 1, sizeof *rules);
    if (!rules) {
        free(patterns), free(symbols);
        return NULL;
    }

    struct gram_packed_spec *spec = malloc(sizeof *spec);
    if (!spec) {
        free(patterns), free(symbols), free(rules);
        return NULL;
    }

    *spec = (struct gram_parsed_spec) {
        .patterns = patterns,
        .symbols = symbols,
        .rules = rules
    };

    return spec;
}

static bool pack_symbols(struct gram_symbol *symbols, struct hash_table *symtab, struct gram_stats stats) {
    struct gram_symbol *sym = NULL;
    struct hash_iterator it = hash_iterator(symtab);
    while ((sym = htnext(NULL, &it))) {
        int i = detnum(sym, stats);
        symbols[i] = *sym;
        symbols[i].num = i;

        if (sym->type == GM_NONTERM) {
            // allocate space for the symbol's derived rules, +1 for -1 end marker
            int *rules = calloc(sym->nrules + 1, sizeof *rules);
            symbols[i].rules = rules;
            // gram_pack frees resources
            if (!rules) return false;
        }
    }

    return true;
}

static bool pack_rhs_pattern(struct regex_pattern *pat, int sym, char *str) {
    // the pattern was already added
    if (pat->sym == sym) return true;

    if (!allocate_pattern(pat, sym, NULL, str))
        return false;

    // FIXME: this wastes two bytes per string
    strip_quotes(pat->pattern);

    return true;
}

struct gram_packed_spec *gram_pack(
    struct gram_parser_spec *spec,
    struct hash_table *symtab,
    struct gram_stats stats
) {
    struct gram_parsed_spec *pspec = gram_parsed_spec(stats);

    // pack the symbols
    if (!pack_symbols(pspec->symbols, symtab, stats))
        goto oom;

    // count the number of pattern terms to compute offsets
    // when traversing the AST
    int pterms = 0;

    // pack the pattern definitions
    struct regex_pattern *pat = pspec->patterns;
    struct gram_pattern_def *pdef = NULL;
    for (pdef = spec->pdefs; pdef; pdef = pdef->next) {
        int sym = 0;
        if (pdef->tag_only) {
            sym = RX_TAG_ONLY;
        } else if (pdef->skip) {
            sym = RX_SKIP;
        } else {
            sym = detnum(htlookup(pdef->id, symtab), stats);
            pterms++;
        }

        if (!allocate_pattern(pat, sym, pdef->id, pdef->regex))
            goto oom;

        pat++;
    }

    int rn = 0; // rule number, for nonterm rules in symbols
    int **r = pspec->rules;
    struct gram_rule *rule = spec->rules;
    for (; rule; rule = rule->next) {
        // for adding rules to the non-terminal's "derives" list (rules)
        int const i = detnum(htlookup(rule->id, symtab), stats);
        struct gram_symbol *ntsym = pspec->symbols[i];

        // TODO:
        // need to figure out how to maintain a pointer to the current derives
        // address, when alts may be spread among multiple non-contiguous rules
        // i.e.
        //
        // foo: bar | baz
        // bar: a b c
        // foo bing
        //
        // foo then needs to have 0, 1, and 3 in it's derives list
        //

        struct gram_alt *alt = rule->alts;

        if (!alt || is_empty_rhs(alt->rhses)) {
            if ((*r++ = calloc(1, sizeof *r)) == NULL)
                goto oom;
            continue;
        }

        for (; alt; rn++, alt = alt->next) {
            struct gram_rhs *rhs = alt->rhses;

            if ((*r = calloc(rhs->n + 1, sizeof **r)) == NULL)
                goto oom;

            int *s = *r;
            for (; rhs; rhs = rhs->next) {
                if (rhs->type == GM_EMPTY_RHS) continue;

                int sym = detnum(htlookup(rhs->str, symtab), stats);

                // add rule symbol
                *s++ = sym;

                // add pattern
                if (rhs->type == GM_CHAR_RHS || rhs->type == GM_STRING_RHS) {
                    // N pattern terminals + M grammar terminals + reserved terminal
                    if (!pack_rhs_pattern(&pat[sym - pterms - 1], sym, rhs->str))
                        goto oom;
                }
            }

            r++;
        }
    }

    return pspec;
oom:
    free_gram_packed_spec(pspec);
    return NULL;
}

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

    struct gram_symbol *sym = symbols[1];
    while (sym->num) {
        if (sym->rules) free(sym->rules);
        sym++;
    }

    free(symbols);
}

static void free_rules(int **rules) {
    if (!rules) return;
    int **r = rules;
    while (*r) free(*r), r++;
    free(rules);
}

void free_gram_packed_spec(struct gram_packed_spec *spec) {
    free_patterns(spec->patterns);
    free_symbols(spec->symbols);
    free_rules(spec->rules);
    free(spec);
}

