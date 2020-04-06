#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/debug.h>
#include <base/hash_table.h>
#include <base/string.h>
#include <regex/base.h>
#include "gram/check.h"
#include "gram/parser.h"
#include "gram/spec.h"
#include "gram/pack.h"

#include "oom.c"

#define debug(...) debug_ns("gram_pack", __VA_ARGS__);

static int detnum(struct gram_symbol_entry *sym, struct gram_stats stats) {
    int i = sym->s.num + 1; // #0 reserved

    if (sym->s.type == GM_NONTERM)
        i += stats.terms;

    return i;
}

static bool alloc_pattern(struct regex_pattern *pat, int sym, char *tag, char *pattern) {
    debug("packing pattern: %d, %s, %s\n", sym, tag, pattern);
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

static bool pack_symbols(unsigned int **rps, struct gram_symbol *symbols, struct hash_table *symtab, struct gram_stats stats) {
    struct gram_symbol_entry *sym = NULL;
    struct hash_iterator it = hash_iterator(symtab);
    while ((sym = htnext(NULL, &it))) {
        if (sym->type != GM_SYMBOL_ENTRY) continue;

        int i = detnum(sym, stats);
        symbols[i] = sym->s;
        symbols[i].num = i;

        if (sym->s.type == GM_NONTERM && sym->nrules) {
            // allocate space for the symbol's derived rules +1 for 0 end marker
            unsigned int *rules = calloc(sym->nrules + 1, sizeof *rules);
            rps[sym->s.num] = rules;
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

    if (!alloc_pattern(pat, sym, NULL, str))
        return false;

    strip_quotes(pat->pattern);

    return true;
}

static bool pack_patterns(int *ntpatterns, struct regex_pattern *patterns, struct hash_table *symtab, struct gram_parser_spec *spec) {
    struct regex_pattern *pat = patterns;
    struct gram_pattern_def *pdef = NULL;
    for (pdef = spec->pdefs; pdef; pdef = pdef->next) {
        int sym = 0;
        if (pdef->tag_only) {
            sym = RX_TAG_ONLY;
            (*ntpatterns)++;
        } else if (pdef->skip) {
            sym = RX_SKIP;
            (*ntpatterns)++;
        } else {
            sym = detnum(htlookup(pdef->id, symtab), spec->stats);
        }

        if (!alloc_pattern(pat, sym, pdef->id, pdef->regex))
            return false;

        strip_quotes(pat->pattern);

        pat++;
    }

    return true;
}

bool gram_pack(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    struct gram_parse_context *context
) {
    assert(spec != NULL);

    if (spec->type == GM_PACKED_SPEC) return true;

    if (spec->type == GM_PARSED_SPEC && !gram_check(error, spec, context))
        return false;

    struct hash_table *symtab = context->symtab;
    struct gram_stats stats = spec->stats;

    // terminated by a null pattern
    struct regex_pattern *patterns = calloc(stats.patterns + 1, sizeof *patterns);
    if (!patterns) return oom_error(error, NULL);

    // terminated by a null symbol, indexed by symbol number, and symbol #0 reserved
    struct gram_symbol *symbols = calloc(stats.terms + stats.nonterms + 2, sizeof (*symbols));
    if (!symbols) return oom_error(error, patterns);

    // terminated by NULL, indexed by rule number, and rule #0 reserved
    unsigned int **rules = calloc(stats.rules + 2, sizeof *rules);
    if (!rules) return oom_error(error, patterns, symbols);

    // nonterm rules positions
    unsigned int **rps = calloc(stats.nonterms, sizeof *rps);

    if (!rps) goto oom;

    // pack the symbols
    if (!pack_symbols(rps, symbols, symtab, stats))
        goto oom;

    // count the number of non-pattern terms to add as an offset
    // when indexing the patterns table in the below AST loop
    int ntpatterns = 0;
    if (!pack_patterns(&ntpatterns, patterns, symtab, spec))
        goto oom;

    int rn = 1; // rule number, for nonterm rules in symbols (rule #0 reserved)
    unsigned int **r = &rules[1];
    struct gram_rule *rule = spec->prules;
    for (; rule; rule = rule->next) {
        // for adding rules to the non-terminal's "derives" list (rules)
        struct gram_symbol_entry *ntsym = htlookup(rule->id, symtab);
        int nti = detnum(ntsym, stats);
        ;

        struct gram_alt *alt = rule->alts;

        for (; alt; rn++, alt = alt->next) {
            *rps[ntsym->s.num]++ = rn;
            struct gram_rhs *rhs = alt->rhses;

            // if an alt contains only $empty, mark the non-terminal as nullable
            //  and allocate a zero rule
            if (is_empty_rhs(rhs)) {
                if ((*r++ = calloc(1, sizeof *r)) == NULL)
                    goto oom;
                symbols[nti].nullable = true;
                continue;
            }

            // otherwise, continue as normal
            if ((*r = calloc(rhs->n + 1, sizeof **r)) == NULL)
                goto oom;

            unsigned int *s = *r;
            for (; rhs; rhs = rhs->next) {
                if (rhs->type == GM_EMPTY_RHS) continue;

                int sym = detnum(htlookup(rhs->str, symtab), stats);

                // add rule symbol
                *s++ = sym;

                // add pattern
                if (rhs->type == GM_CHAR_RHS || rhs->type == GM_STRING_RHS) {
                    // sym num + num non-terminal patterns (skip/to) + reserved terminals 0 and eof
                    if (!pack_rhs_pattern(&patterns[sym + ntpatterns - 2], sym, rhs->str))
                        goto oom;
                }
            }

            r++;
        }
    }

    free(rps);

    free_gram_parser_spec(spec);
    *spec = gram_packed_spec(patterns, symbols, rules, spec->start_rule + 1, stats);

    return true;
oom:
    return oom_error(error, patterns, symbols, rules, rps);
}

