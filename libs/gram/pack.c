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

// symbols and rules list begins at index #1
#define offs(n) ((n) + 1)
// symbols, patterns, and rules are all null terminated
#define nullterm(n) ((n) + 1)

static unsigned int detnum(struct gram_symbol_entry *sym, struct gram_stats stats) {
    unsigned int i = sym->s.num; // #0 reserved

    if (sym->s.type == GM_NONTERM)
        i += stats.terms;
    else
        i += 1; // eof

    return offs(i);
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

static bool pack_symbols(unsigned int **rps, struct gram_symbol *symbols, struct hash_table *symtab, struct gram_stats stats) {
    // eof symbol
    symbols[GM_EOF] = (struct gram_symbol) { GM_TERM, GM_EOF };

    struct gram_symbol_entry *sym = NULL;
    struct hash_iterator it = hash_iterator(symtab);
    while ((sym = htnext(NULL, &it))) {
        if (sym->type != GM_SYMBOL_ENTRY) continue;

        int i = detnum(sym, stats);
        symbols[i] = sym->s;
        symbols[i].num = i;

        if (sym->s.type == GM_NONTERM && sym->nderives) {
            // allocate space for the symbol's derived rules +1 for 0 end marker
            unsigned int *derives = calloc(nullterm(sym->nderives), sizeof *derives);
            rps[sym->s.num] = derives;
            symbols[i].derives = derives;
            // gram_pack frees resources
            if (!derives) return false;
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

unsigned int *alloc_rule(size_t n) {
    return calloc(n, sizeof (unsigned int));
}

bool gram_pack(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    struct gram_parse_context *context
) {
    assert(spec != NULL);

    if (spec->type == GM_PACKED_SPEC) return true;

    if (spec->type == GM_PARSED_SPEC && !gram_check(error, spec, context))
        return false;

    // start rule, empty rule
    spec->stats.rules += 2;

    // eof symbol
    spec->stats.terms++;
    spec->stats.symbols++;

    struct hash_table *symtab = context->symtab;
    struct gram_stats stats = spec->stats;

    struct regex_pattern *patterns = calloc(nullterm(stats.patterns), sizeof *patterns);
    if (!patterns) return oom_error(error, NULL);

    struct gram_symbol *symbols = calloc(offs(nullterm(stats.symbols)), sizeof (*symbols));
    if (!symbols) return oom_error(error, patterns);

    unsigned int **rules = calloc(offs(nullterm(stats.rules)), sizeof *rules);
    if (!rules) return oom_error(error, patterns, symbols);

    // nonterm rules positions
    unsigned int **rps = calloc(stats.nonterms, sizeof *rps);

    if (!rps) goto oom;

    if (!pack_symbols(rps, symbols, symtab, stats))
        goto oom;

    // NTPATTERNS count the number of non-pattern terms to add as an offset
    // when indexing the patterns table in the below AST loop
    int ntpatterns = 0;
    if (!pack_patterns(&ntpatterns, patterns, symtab, spec))
        goto oom;

    // create an empty rule
    unsigned int const empty_rule = stats.rules;
    if ((rules[empty_rule] = alloc_rule(1)) == NULL)
        goto oom;

    // create the start rule
    rules[GM_START] = alloc_rule(3);
    rules[GM_START][0] = offs(stats.terms);
    rules[GM_START][1] = GM_EOF;

    // fill in the rules lists and nonterm derives lists
    int rn = 2;
    unsigned int **r = &rules[2];
    struct gram_rule *rule = spec->prules;
    for (; rule; rule = rule->next) {
        struct gram_symbol_entry *ntsym = htlookup(rule->id, symtab);
        struct gram_alt *alt = rule->alts;

        for (; alt; alt = alt->next) {
            struct gram_rhs *rhs = alt->rhses;

            if (gram_rhses_empty(rhs)) {
                *rps[ntsym->s.num]++ = empty_rule;
                continue;
            }

            *rps[ntsym->s.num]++ = rn++;

            if ((*r = alloc_rule(nullterm(rhs->n))) == NULL)
                goto oom;

            unsigned int *s = *r;
            for (; rhs; rhs = rhs->next) {
                if (rhs->type == GM_EMPTY_RHS) continue;

                int sym = detnum(htlookup(rhs->str, symtab), stats);

                // add rule symbol
                *s++ = sym;

                // add pattern
                if (rhs->type == GM_CHAR_RHS || rhs->type == GM_STRING_RHS) {
                    // sym num + num non-terminal patterns (skip/to) + reserved symbol 0
                    if (!pack_rhs_pattern(&patterns[sym + ntpatterns - 2], sym, rhs->str))
                        goto oom;
                }
            }

            r++;
        }
    }

    free(rps);

    free_gram_parser_spec(spec);
    *spec = gram_packed_spec(patterns, symbols, rules, stats);

    return true;
oom:
    return oom_error(error, patterns, symbols, rules, rps);
}

