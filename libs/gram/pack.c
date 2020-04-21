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

#include "internal/spec.c"
#include "internal/oom.c"

#define debug(...) debug_ns("gram_pack", __VA_ARGS__);

// symbols and rules list begins at index #1
#define offs(n) ((n) + 1)
// symbols, patterns, and rules are all null terminated
#define nullterm(n) ((n) + 1)

static gram_sym_no detnum(struct gram_symbol_entry *sym, struct gram_stats stats) {
    gram_sym_no i = sym->s.num; // #0 reserved

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

static struct gram_symbol *pack_symbols(gram_rule_no **dps, struct hash_table *symtab, struct gram_stats stats) {
    struct gram_symbol *symbols = calloc(offs(nullterm(stats.symbols)), sizeof (*symbols));
    if (!symbols) return NULL;

    // eof symbol
    symbols[GM_EOF] = (struct gram_symbol) { GM_TERM, GM_EOF };

    bool success = true;
    struct gram_symbol_entry *sym = NULL;
    struct hash_iterator it = hash_iterator(symtab);
    while ((sym = htnext(NULL, &it))) {
        if (sym->type != GM_SYMBOL_ENTRY) continue;

        gram_sym_no i = detnum(sym, stats);
        symbols[i] = sym->s;
        symbols[i].num = i;

        if (sym->s.type == GM_NONTERM && sym->nderives) {
            // allocate space for the symbol's derived rules +1 for 0 end marker
            gram_rule_no *derives = calloc(nullterm(sym->nderives), sizeof *derives);
            dps[sym->s.num] = derives;
            symbols[i].derives = derives;

            if (!derives) {
                success = false;
                break;
            }
        }
    }

    if (!success) {
        free_symbols(symbols);
        return NULL;
    }

    return symbols;
}

static bool pack_rhs_pattern(struct regex_pattern *pat, int sym, char *str) {
    // the pattern was already added
    if (pat->sym == sym) return true;

    if (!alloc_pattern(pat, sym, NULL, str))
        return false;

    strip_quotes(pat->pattern);

    return true;
}

static struct regex_pattern *pack_patterns(int *ntpatterns, struct hash_table *symtab, struct gram_stats stats, struct gram_parser_spec *spec) {
    struct regex_pattern *patterns = calloc(nullterm(stats.patterns), sizeof *patterns);
    if (!patterns) return NULL;

    bool success = true;
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
            sym = detnum(htlookup(pdef->id, symtab), stats);
        }

        if (!alloc_pattern(pat, sym, pdef->id, pdef->regex)) {
            success = false;
            break;
        }

        strip_quotes(pat->pattern);
        pat++;
    }

    if (!success) {
        free_patterns(patterns);
        return NULL;
    }

    return patterns;
}

gram_sym_no *alloc_rule(size_t n) {
    return calloc(n, sizeof (gram_sym_no));
}

static gram_sym_no **pack_rules(
    gram_rule_no **dps, int ntpatterns, struct regex_pattern *patterns,
    struct hash_table *symtab, struct gram_stats stats, struct gram_parser_spec *spec
) {
    gram_sym_no **rules = calloc(offs(nullterm(stats.rules)), sizeof *rules);
    if (!rules) return NULL;

    // create an empty rule
    // this makes computing first/follow for rules less than ideal
    // but the actual parser generation doesn't require nonterms to have unique rules
    gram_rule_no const empty_rule = stats.rules;
    if ((rules[empty_rule] = alloc_rule(1)) == NULL)
        return free(rules), NULL;

    // create the start rule
    if ((rules[GM_START] = alloc_rule(3)) == NULL)
        goto oom;

    rules[GM_START][0] = offs(stats.terms);
    rules[GM_START][1] = GM_EOF;

    // fill in the rules lists and nonterm derives lists
    gram_rule_no rn = 2;
    gram_sym_no **r = &rules[rn];
    struct gram_rule *rule = spec->prules;
    for (; rule; rule = rule->next) {
        struct gram_symbol_entry *ntsym = htlookup(rule->id, symtab);
        struct gram_alt *alt = rule->alts;

        for (; alt; alt = alt->next) {
            struct gram_rhs *rhs = alt->rhses;

            if (gram_rhses_empty(rhs)) {
                *dps[ntsym->s.num]++ = empty_rule;
                continue;
            }

            *dps[ntsym->s.num]++ = rn++;

            if ((*r = alloc_rule(nullterm(rhs->n))) == NULL)
                goto oom;

            gram_sym_no *s = *r;
            for (; rhs; rhs = rhs->next) {
                if (rhs->type == GM_EMPTY_RHS) continue;

                gram_sym_no sym = detnum(htlookup(rhs->str, symtab), stats);

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

    return rules;
oom:
    free(rules[empty_rule]);
    free_rules(rules);
    return NULL;
}

bool gram_pack(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    struct gram_spec_parser *parser
) {
    assert(spec != NULL);

    if (spec->type == GM_PACKED_SPEC) return true;

    if (spec->type == GM_PARSED_SPEC && !gram_check(error, spec, parser))
        return false;

    struct hash_table *symtab = parser->symtab;
    struct gram_stats stats = spec->stats;

    // account for eof symbol
    stats.terms++;
    stats.symbols++;

    // account for start and empty rule
    stats.rules += 2;

    // nonterm derives positions
    gram_rule_no **dps = calloc(stats.nonterms, sizeof *dps);
    if (!dps) return oom_error(error, NULL);

    struct gram_symbol *symbols = NULL;
    if ((symbols = pack_symbols(dps, symtab, stats)) == NULL)
        return oom_error(error, dps);

    // NTPATTERNS count the number of non-pattern terms to add as an offset when indexing the patterns table in the below AST loop
    struct regex_pattern *patterns = NULL;
    int ntpatterns = 0;
    if ((patterns = pack_patterns(&ntpatterns, symtab, stats, spec)) == NULL) {
        free_symbols(symbols);
        return oom_error(error, dps);
    }

    gram_sym_no **rules = NULL;
    if ((rules = pack_rules(dps, ntpatterns, patterns, symtab, stats, spec)) == NULL) {
        free_symbols(symbols);
        free_patterns(patterns);
        return oom_error(error, dps);
    }

    free(dps);
    free_gram_parser_spec(spec);
    *spec = gram_packed_spec(patterns, symbols, rules, stats);

    return true;
}

