#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <base/debug.h>
#include <base/string.h>
#include <base/bitset.h>
#include <regex/base.h>
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"
#include "internal/spec.c"
#include "internal/macros.c"

#define debug(...) debug_ns("gram_analyze", __VA_ARGS__);

void gram_count(struct gram_parser_spec *spec) {
    assert(spec != NULL);
    assert(spec->type == GM_PACKED_SPEC);

    struct gram_stats stats = { 0 };

    if (spec->patterns) {
        struct regex_pattern *pattern = spec->patterns;
        while (!regex_null_pattern(pattern))
            stats.patterns++, pattern++;
    }

    if (spec->symbols) {
        struct gram_symbol *sym = gram_symbol0(spec);
        while (!gram_symbol_null(sym)) {
            if (sym->type == GM_TERM) stats.terms++;
            else stats.nonterms++;
            stats.symbols++;
            sym++;
        }
    }

    if (spec->rules) {
        gram_sym_no **rule = gram_rule0(spec);
        while (*rule) {
            gram_sym_no *s = *rule;
            while (*s) stats.rsymbols++, s++;
            stats.rules++;
            rule++;
        }
    }

    spec->stats = stats;
}

#define STATS_TITLE_FMT "stats:\n"
#define STATS_FMT "  patterns: %d\n  terms: %d\n  nonterms: %d\n  symbols: %d\n  rules: %d\n  rsymbols: %d\n"
void print_gram_stats(FILE *handle, struct gram_stats const stats) {
    fprintf(handle, STATS_TITLE_FMT);
    fprintf(handle, STATS_FMT,
        stats.patterns,
        stats.terms,
        stats.nonterms,
        stats.symbols,
        stats.rules,
        stats.rsymbols);
    fprintf(handle, "\n");
}

static void free_sets(struct bitset **sets, unsigned n) {
    if (!sets) return;

    for (unsigned i = 1; i < n; i++) {
        free(sets[i]);
    }
    free(sets);
}

#define NULLABLE_HEADER_FMT "  %4s  %s\n"
#define NULLABLE_ROW_FMT    "  %4d  %s\n"
static void print_nullable(FILE *handle, bool const *nullable, unsigned n) {
    if (!nullable) return;
    fprintf(handle, NULLABLE_HEADER_FMT, "num", "nullable");
    for (unsigned i = 1; i < n; i++) {
        fprintf(handle, NULLABLE_ROW_FMT, i, yesno(nullable[i]));
    }
    fprintf(handle, "\n");
}

static void print_sets(FILE *handle, struct bitset **sets, unsigned n) {
    if (!sets) return;

    fprintf(handle, "  %4s  %-s\n", "num", "set");
    for (unsigned i = 1; i < n; i++) {
        fprintf(handle, "  %4d  ", i);
        print_bitset(handle, sets[i]);
        fprintf(handle, "\n");
    }
    fprintf(handle, "\n");
}

struct bitset **alloc_sets(unsigned nsets, unsigned size) {
    struct bitset **sets = calloc(nsets, sizeof *sets);
    if (!sets) return NULL;

    for (unsigned i = 1; i < nsets; i++) {
        if (!(sets[i] = bitset(size)))
            return free_sets(sets, i), NULL;
    }

    return sets;
}

#define SYMBOL_NULLABLE(name) \
static bool name( \
    bool *added, bool *nullable, gram_sym_no s, struct gram_parser_spec const *spec \
)
#define RULE_NULLABLE(name) \
static bool name( \
    bool *added, bool *nullable, gram_rule_no r, struct gram_parser_spec const *spec \
)

SYMBOL_NULLABLE(symbol_nullable);
RULE_NULLABLE(rule_nullable);

SYMBOL_NULLABLE(symbol_nullable) {
    invariant(assert_symbol_index, s, spec);
    struct gram_symbol sym = spec->symbols[s];

    if (added[sym.num]) return nullable[sym.num];
    added[sym.num] = true;

    bool snull = false;

    if (sym.type == GM_NONTERM) {
        gram_rule_no *r = sym.derives;
        while (*r) {
            bool rnull = rule_nullable(added, nullable, *r, spec);
            snull = snull || rnull; // a non-terminal is nullable if any of it's rules are nullable
            r++;
        }
    }

    nullable[sym.num] = snull;

    return snull;
}

RULE_NULLABLE(rule_nullable) {
    invariant(assert_rule_index, r, spec);
    gram_sym_no *s = spec->rules[r];
    bool rnull = true;
    while (*s) {
        // added determines whether we're visiting symbols or not
        bool snull = added ? symbol_nullable(added, nullable, *s, spec) : nullable[*s];
        rnull = rnull && snull; // a rule is nullable if all of it's symbols are nullable
        s++;
    }
    return rnull;
}

static bool *symbols_nullable(struct gram_parser_spec const *spec) {
    unsigned nsymbols = offs(spec->stats.symbols);
    bool *nullable = calloc(nsymbols, sizeof *nullable);
    if (!nullable) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free(nullable), NULL;

    struct gram_symbol *start = gram_start_sym(spec);
    symbol_nullable(added, nullable, start->num, spec);

    free(added);

    return nullable;
}

#define RULE_FIRST(name) \
static struct bitset *name( \
    bool *added, gram_sym_no fsym, struct bitset **firsts, bool *nullable, \
    gram_rule_no r, struct gram_parser_spec const *spec \
)
#define SYMBOL_FIRST(name) \
static void name( \
    bool *added, gram_sym_no fsym, struct bitset **firsts, bool *nullable, \
    gram_sym_no s, struct gram_parser_spec const *spec \
)

RULE_FIRST(rule_first);
SYMBOL_FIRST(symbol_first);

SYMBOL_FIRST(symbol_first) {
    invariant(assert_symbol_index, s, spec);

    struct gram_symbol sym = spec->symbols[s];

    if (added[s]) return;
    added[s] = true;

    if (sym.type == GM_TERM) {
        bsins(sym.num, firsts[fsym]);
    } else {
        gram_rule_no *r = sym.derives;
        while (*r) rule_first(added, fsym, firsts, nullable, *r, spec), r++;
    }
}

RULE_FIRST(rule_first) {
    invariant(assert_rule_index, r, spec);

    struct bitset *rfirst = NULL;

    if (!added && !(rfirst = bitset(spec->stats.terms)))
        return NULL;

    gram_sym_no *s = spec->rules[r];

    while (*s) {
        if (added) symbol_first(added, fsym, firsts, nullable, *s, spec);
        else       bsunion(rfirst, firsts[*s]);

        if (!nullable[*s]) break;

        s++;
    }

    return rfirst;
}

static struct bitset **symbols_firsts(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    unsigned nsymbols = an->nsymbols;
    struct bitset **firsts = alloc_sets(nsymbols, spec->stats.terms);
    if (!firsts) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free_sets(firsts, nsymbols), NULL;

    struct gram_symbol *s = gram_symbol0(spec);
    while (!gram_symbol_null(s)) {
        symbol_first(added, s->num, firsts, an->nullable, s->num, spec);
        memset(added, false, nsymbols);
        s++;
    }

    free(added);

    return firsts;
}

#define RULE_FOLLOWS(name) \
static void name( \
    bool *added, struct bitset **follows, struct gram_symbol_analysis *an,  \
    gram_rule_no r, gram_sym_no nt, struct gram_parser_spec const *spec \
)

#define SYMBOL_FOLLOWS(name) \
static void name( \
    bool *added, struct bitset **follows, struct gram_symbol_analysis *an, \
    gram_sym_no s, struct gram_parser_spec const *spec \
)

RULE_FOLLOWS(rule_follows);
SYMBOL_FOLLOWS(_symbol_follows);

SYMBOL_FOLLOWS(_symbol_follows) {
    invariant(assert_symbol_index, s, spec);
    struct gram_symbol sym = spec->symbols[s];

    if (added[sym.num]) return;
    added[sym.num] = true;

    if (sym.type == GM_NONTERM) {
        gram_rule_no *r = sym.derives;
        while (*r) {
            rule_follows(added, follows, an, *r, sym.num, spec);
            r++;
        }
    }
}

RULE_FOLLOWS(rule_follows) {
    invariant(assert_rule_index, r, spec);
    gram_sym_no *s = spec->rules[r];
    while (*s) {
        gram_sym_no *n = s + 1;

        // add the first sets for the symbols up to the first non-nullable symbol
        while (*n) {
            bsunion(follows[*s], an->firsts[*n]);
            if (!an->nullable[*n]) break;
            n++;
        }

        // if we're at the end, add the follow set for the current non-terminal
        if (!*n) bsunion(follows[*s], follows[nt]);

        _symbol_follows(added, follows, an, *s, spec);

        s++;
    }
}

static size_t all_sets_size(struct bitset **follows, struct gram_parser_spec const *spec) {
    size_t size = 0;

    FOR_SYMBOL(spec->stats, s) {
        size += bssize(follows[s]);
    }

    return size;
}

static size_t
symbol_follows(
    bool *added, struct bitset **follows, struct gram_symbol_analysis *an,
    gram_sym_no s, struct gram_parser_spec const *spec
) {
    _symbol_follows(added, follows, an, s, spec);
    return all_sets_size(follows, spec);
}

static struct bitset **symbols_follows(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    unsigned nsymbols = an->nsymbols;

    struct bitset **follows = alloc_sets(nsymbols, spec->stats.terms);
    if (!follows) return NULL;

    bool *added = calloc(nsymbols, sizeof *added);
    if (!added) return free_sets(follows, nsymbols), NULL;

    struct gram_symbol *start = gram_start_sym(spec);

    if (start->num != GM_EOF)
        bsins(GM_EOF, follows[start->num]);

    // use cardinality of all sets to compute equivalence, and continue until we find the least fixed point
    size_t p = 1, n;
    int passes = 1;
    while ((n = symbol_follows(added, follows, an, start->num, spec)) != p) {
        p = n;
        memset(added, false, nsymbols);
        passes++;
    }

    debug("computing the follow sets required %d total passes\n", passes);

    free(added);

    return follows;
}

bool gram_analyze_symbols(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    free_gram_symbol_analysis(an);

    an->nsymbols = offs(spec->stats.symbols);

    an->nullable = symbols_nullable(spec);
    if (!an->nullable) return false;

    an->firsts = symbols_firsts(an, spec);
    if (!an->firsts) return free_gram_symbol_analysis(an), false;

    an->follows = symbols_follows(an, spec);
    if (!an->follows) return free_gram_symbol_analysis(an), false;

    return true;
}

void free_gram_symbol_analysis(struct gram_symbol_analysis *an) {
    if (!an) return;

    if (an->nullable) free(an->nullable);
    if (an->firsts) free_sets(an->firsts, an->nsymbols);
    if (an->follows) free_sets(an->follows, an->nsymbols);

    *an = (struct gram_symbol_analysis) { 0 };
}

void print_gram_symbol_analysis(FILE *handle, struct gram_symbol_analysis const *an) {
    assert(an != NULL);

    fprintf(handle, "symbol nullable:\n");
    print_nullable(handle, an->nullable, an->nsymbols);

    fprintf(handle, "symbol firsts:\n");
    print_sets(handle, an->firsts, an->nsymbols);

    fprintf(handle, "symbol follows:\n");
    print_sets(handle, an->follows, an->nsymbols);
}

static bool *rules_nullable(struct gram_symbol_analysis *san, struct gram_parser_spec const *spec) {
    unsigned const nrules = offs(spec->stats.rules);
    bool *rnullable = calloc(nrules, sizeof *rnullable);
    if (!rnullable) return NULL;

    for (unsigned r = 1; r < nrules; r++)
        rnullable[r] = rule_nullable(NULL, san->nullable, r, spec);

    return rnullable;
}

static struct bitset **rules_firsts(struct gram_symbol_analysis *san, struct gram_parser_spec const *spec) {
    unsigned const nrules = offs(spec->stats.rules);
    struct bitset **firsts = calloc(nrules, sizeof *firsts);
    if (!firsts) return NULL;

    for (unsigned r = 1; r < nrules; r++)
        firsts[r] = rule_first(NULL, 0, san->firsts, san->nullable, r, spec);

    return firsts;
}

bool gram_analyze_rules(struct gram_rule_analysis *an, struct gram_symbol_analysis *san, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    free_gram_rule_analysis(an);

    an->nrules = offs(spec->stats.rules);

    an->nullable = rules_nullable(san, spec);
    if (!an->nullable) return false;

    an->firsts = rules_firsts(san, spec);
    if (!an->firsts) return false;

    return true;
}

void free_gram_rule_analysis(struct gram_rule_analysis *an) {
    if (!an) return;

    if (an->nullable) free(an->nullable);
    if (an->firsts) free_sets(an->firsts, an->nrules);
}

void print_gram_rule_analysis(FILE *handle, struct gram_rule_analysis const *an) {
    assert(an != NULL);

    fprintf(handle, "rule nullable:\n");
    print_nullable(handle, an->nullable, an->nrules);

    fprintf(handle, "rule firsts:\n");
    print_sets(handle, an->firsts, an->nrules);
}

static struct gram_conflict *first_first_conflict(
    struct gram_conflict *last, gram_sym_no nt, gram_rule_no r1, gram_rule_no r2,
    struct gram_analysis *an
) {
    struct gram_conflict *conf = malloc(sizeof *conf);
    if (!conf) return NULL;

    *conf = (struct gram_conflict) {
        GM_FIRST_FIRST,
        .nonterm = nt,
        .rules = { offs(r1), offs(r2) }
    };

    if (last) last->next = conf;
    else an->conflicts = conf;

    if (an->clas >= GM_LL) an->clas--;

    return conf;
}

static struct gram_conflict *first_follows_conflict(
    struct gram_conflict *last, gram_sym_no nt, gram_rule_no r,
    struct gram_analysis *an
) {
    struct gram_conflict *conf = malloc(sizeof *conf);
    if (!conf) return NULL;

    *conf = (struct gram_conflict) {
        GM_FIRST_FOLLOWS,
        .nonterm = nt,
        .rule = offs(r)
    };

    if (last) last->next = conf;
    else an->conflicts = conf;

    if (an->clas >= GM_LL) an->clas--;

    return conf;
}

static struct gram_conflict *null_ambiguity_conflict(
    struct gram_conflict *last, gram_sym_no nt,
    struct gram_analysis *an
) {
    struct gram_conflict *conf = malloc(sizeof *conf);
    if (!conf) return NULL;

    *conf = (struct gram_conflict) {
        GM_NULL_AMBIGUITY,
        .nonterm = nt
    };

    if (last) last->next = conf;
    else an->conflicts = conf;

    if (an->clas >= GM_LL) an->clas--;

    return conf;
}

static struct gram_conflict *left_recursion_conflict(
    struct gram_conflict *last, struct array *derivs,
    struct gram_analysis *an
) {
    struct gram_conflict *conf = malloc(sizeof *conf);
    if (!conf) return NULL;

    gram_sym_no *dlist = alist(derivs);
    if (!dlist) return free(conf), NULL;

    *conf = (struct gram_conflict) {
        GM_LEFT_RECURSION,
        .derivations = dlist,
        .n = asize(derivs)
    };

    if (last) last->next = conf;
    else an->conflicts = conf;

    if (an->clas >= GM_LL) an->clas--;

    return conf;
}

#define RULE_LEFT_RECURSION(name) \
static struct gram_conflict *name( \
    struct gram_conflict *last, struct gram_analysis *an, struct gram_symbol_analysis *san, \
    bool *added, struct array *derivs, gram_rule_no r, gram_sym_no const nt, struct gram_parser_spec const *spec \
)

#define SYMBOL_LEFT_RECURSION(name) \
static struct gram_conflict *name( \
    struct gram_conflict *last, struct gram_analysis *an, struct gram_symbol_analysis *san, \
    bool *added, struct array *derivs, gram_sym_no s, gram_sym_no const nt, struct gram_parser_spec const *spec \
)

RULE_LEFT_RECURSION(rule_left_recursion);
SYMBOL_LEFT_RECURSION(symbol_left_recursion);

RULE_LEFT_RECURSION(rule_left_recursion) {
    invariant(assert_rule_index, r, spec);

    gram_sym_no *s = spec->rules[r];

    bool *nullable = san->nullable;

    while (*s && (last = symbol_left_recursion(last, an, san, added, derivs, *s, nt, spec))) {
        if (!nullable[*s]) break;
        s++;
    }

    return last;
}

SYMBOL_LEFT_RECURSION(symbol_left_recursion) {
    invariant(assert_symbol_index, s, spec);

    struct gram_symbol sym = spec->symbols[s];

    if (added[sym.num]) {
        if (sym.type == GM_NONTERM && sym.num == nt) {
            apush(&sym.num, derivs);
            last = left_recursion_conflict(last, derivs, an);
            apop(&sym.num, derivs);
        }
        return last;
    }

    added[sym.num] = true;

    if (sym.type == GM_NONTERM) {
        apush(&sym.num, derivs);

        gram_rule_no *r = sym.derives;
        while (*r && (last = rule_left_recursion(last, an, san, added, derivs, *r, nt, spec)))
            r++;

        apop(&sym.num, derivs);
    }

    return last;
}

static unsigned nderives(gram_rule_no *r) {
    unsigned n = 0;
    while (*r) n++, r++;
    return n;
}

#define DERIVS_STACK_SIZE 5
bool gram_analyze(
    struct gram_analysis *an, struct gram_symbol_analysis *san,
    struct gram_parser_spec const *spec
) {
    invariant(assert_packed_spec, spec);

    free_gram_analysis(an);

    an->clas = GM_LL;

    struct gram_rule_analysis ran = { 0 };

    if (!gram_analyze_rules(&ran, san, spec))
        return false;

    bool *rnullable = ran.nullable,
         *snullable = san->nullable;
    struct bitset **rfirsts = ran.firsts,
                  **sfollows = san->follows;

    struct gram_conflict *conflict = NULL;
    struct array *derivs = NULL;
    bool *added = NULL;

    derivs = init_array(sizeof (gram_sym_no), DERIVS_STACK_SIZE, 0, 0);
    if (!derivs) goto free;

    unsigned const nsymbols = offs(spec->stats.symbols);
    added = calloc(nsymbols, sizeof *added);
    if (!added) goto free;

    if (gram_exists(spec)) {
        struct gram_symbol *nt = gram_nonterm0(spec);
        while (!gram_symbol_null(nt)) {
            gram_rule_no *rules = nt->derives;
            unsigned n = nderives(rules);
            unsigned nnulls = 0;

            for (unsigned i = 0; i < n; i++) {
                gram_rule_no r = rules[i];

                if (rnullable[r]) nnulls++;

                // first-first conflicts
                for (unsigned j = i + 1; j < n; j++) {
                    gram_rule_no t = rules[j];

                    if (!bsdisjoint(rfirsts[r], rfirsts[t])) {
                        if (!(conflict = first_first_conflict(conflict, nt->num, i, j, an)))
                            goto free;
                    }
                }

                if (!snullable[nt->num]) continue;

                // first-follows conflicts
                if (!bsdisjoint(rfirsts[r], sfollows[nt->num])) {
                    if (!(conflict = first_follows_conflict(conflict, nt->num, i, an)))
                        goto free;
                }
            }

            // null ambiguity
            if (nnulls > 1 && !(conflict = null_ambiguity_conflict(conflict, nt->num, an)))
                goto free;

            // left recursion
            conflict = symbol_left_recursion(conflict, an, san, added, derivs, nt->num, nt->num, spec);
            memset(added, false, nsymbols);
            areset(derivs);

            nt++;
        }
    }

    free_gram_rule_analysis(&ran);
    free_array(derivs);
    free(added);

    return true;
free:
    free_gram_analysis(an);
    free_gram_rule_analysis(&ran);
    free_array(derivs);
    free(added);
    return false;
}

void free_gram_analysis(struct gram_analysis *an) {
    if (!an) return;

    struct gram_conflict *conf, *next;

    for (conf = an->conflicts; conf; conf = next) {
        next = conf->next;
        if (conf->type == GM_LEFT_RECURSION)
            free(conf->derivations);
        free(conf);
    }

    *an = (struct gram_analysis) { 0 };
}

static char *sym_str(gram_sym_no s, struct gram_parser_spec const *spec) {
    struct gram_symbol sym = spec->symbols[s];
    if (sym.str) return sym.str;
    static char str[11] = "";
    sprintf(str, "%d", s);
    return str;
}

#define GRAMMAR_TITLE_FMT "grammar:\n"
#define HEADER_INDENT_FMT "  "
#define CONF_INDENT_FMT "    "
#define CONFLICTS_TITLE_FMT "conflicts:\n"
#define FIRST_FIRST_FMT "first-first conflict for non-terminal %s on rules %u and %u\n"
#define FIRST_FOLLOWS_FMT "first-follows conflict for non-terminal %s on rule %u\n"
#define NULL_AMBIGUITY_FMT "non-terminal %s is null ambiguous\n"
#define LEFT_RECURSION_FMT "non-terminal %s is left recursive: "
void print_gram_analysis(FILE *handle, struct gram_analysis *an, struct gram_parser_spec const *spec) {
    assert(an != NULL);

    fprintf(handle, GRAMMAR_TITLE_FMT);

    char *clas = NULL;
    switch (an->clas) {
        case GM_LL: clas = "LL"; break;
        case GM_SLR: clas = "SLR"; break;
        case GM_LR1: clas = "LR1"; break;
        default: break;
    }

    if (clas) {
        fprintf(handle, HEADER_INDENT_FMT);
        fprintf(handle, "class: %s\n", clas);
    }

    if (!an->conflicts) {
        fprintf(handle, "\n");
        return;
    }

    fprintf(handle, HEADER_INDENT_FMT);
    fprintf(handle, CONFLICTS_TITLE_FMT);

    int n = 1;
    for (struct gram_conflict *conf = an->conflicts; conf; conf = conf->next, n++) {
        fprintf(handle, CONF_INDENT_FMT);
        fprintf(handle, "%3d. ", n);

        switch (conf->type) {
            case GM_FIRST_FIRST:
                fprintf(handle, FIRST_FIRST_FMT, sym_str(conf->nonterm, spec), conf->rules[0], conf->rules[1]);
                break;
            case GM_FIRST_FOLLOWS:
                fprintf(handle, FIRST_FOLLOWS_FMT, sym_str(conf->nonterm, spec), conf->rule);
                break;
            case GM_NULL_AMBIGUITY:
                fprintf(handle, NULL_AMBIGUITY_FMT, sym_str(conf->nonterm, spec));
                break;
            case GM_LEFT_RECURSION:
                fprintf(handle, LEFT_RECURSION_FMT, sym_str(conf->derivations[0], spec));
                fprintf(handle, "%s", sym_str(conf->derivations[0], spec));
                for (unsigned i = 1; i < conf->n; i++) {
                    fprintf(handle, " -> %s", sym_str(conf->derivations[i], spec));
                }
                fprintf(handle, "\n");
                break;
        }
    }

    fprintf(handle, "\n");
}

