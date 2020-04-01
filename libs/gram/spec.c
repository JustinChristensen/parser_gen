#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <base/base.h>
#include <base/debug.h>
#include <base/hash_table.h>
#include <base/string.h>
#include <regex/nfa.h>
#include "gram/spec.h"

#ifdef INVARIANTS
void assert_gram_parsed_spec(struct gram_parser_spec const *spec) {
    assert(spec != NULL);
    assert(spec->type == GM_PARSED_SPEC);
}

void assert_gram_packed_spec(struct gram_parser_spec const *spec) {
    assert(spec != NULL);
    assert(spec->type == GM_PACKED_SPEC);

    assert(spec->symbols != NULL);
    assert(spec->symbols[0].num == 0);
    assert(spec->symbols[0].rules == NULL);

    assert(spec->rules != NULL);
    assert(spec->rules[0] == NULL);
}
#endif

static bool _oom_error(struct gram_pack_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    *(error) = (struct gram_pack_error) { .type = GM_PACK_OOM_ERROR, .file = file, .col = col };

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

struct gram_parser_spec gram_parsed_spec(
    struct gram_pattern_def *pdefs,
    struct gram_rule *rules,
    struct gram_stats stats
) {
    return (struct gram_parser_spec) {
        GM_PARSED_SPEC,
        .pdefs = pdefs, .prules = rules,
        .stats = stats
    };
}

struct gram_parser_spec gram_packed_spec(
    struct regex_pattern *patterns, struct gram_symbol *symbols,
    int **rules, struct gram_stats stats
) {
    return (struct gram_parser_spec) {
        GM_PACKED_SPEC,
        .patterns = patterns, .symbols = symbols, .rules = rules,
        .stats = stats
    };
}

static int detnum(struct gram_symbol *sym, struct gram_stats stats) {
    int i = sym->num + 1; // #0 reserved

    if (sym->type == GM_NONTERM)
        i += stats.terms;

    return i;
}

static bool alloc_pattern(struct regex_pattern *pat, int sym, char *tag, char *pattern) {
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

static bool pack_symbols(struct gram_symbol *symbols, struct hash_table *symtab, struct gram_stats stats, int **rps) {
    struct gram_symbol *sym = NULL;
    struct hash_iterator it = hash_iterator(symtab);
    while ((sym = htnext(NULL, &it))) {
        int i = detnum(sym, stats);
        symbols[i] = *sym;
        symbols[i].num = i;

        if (sym->type == GM_NONTERM && sym->nrules) {
            // allocate space for the symbol's derived rules +1 for 0 end marker
            int *rules = calloc(sym->nrules + 1, sizeof *rules);
            rps[sym->num] = rules;
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

    // FIXME: this wastes two bytes per string
    strip_quotes(pat->pattern);

    return true;
}

bool gram_pack(
    struct gram_pack_error *error,
    struct gram_parser_spec *spec,
    struct hash_table *symtab
) {
    assert(spec != NULL);

    if (spec->type == GM_PACKED_SPEC) return true;

    // if (spec->type == GM_PARSED_SPEC && !gram_check(&error->checkerr, spec, symtab)) {
    //     error->type = GM_PACK_CHECK_ERROR;
    //     return false;
    // }

    struct gram_stats stats = spec->stats;

    // terminated by a null pattern
    struct regex_pattern *patterns = calloc(stats.patterns + 1, sizeof *patterns);
    if (!patterns) return oom_error(error, NULL);

    // terminated by a null symbol, indexed by symbol number, and symbol #0 reserved
    struct gram_symbol *symbols = calloc(stats.terms + stats.nonterms + 2, sizeof (*symbols));
    if (!symbols) return oom_error(error, patterns);

    // terminated by NULL, indexed by rule number, and rule #0 reserved
    int **rules = calloc(stats.rules + 2, sizeof *rules);
    if (!rules) return oom_error(error, patterns, symbols);

    // rules positions
    // FIXME: because rules may not be contiguous in the spec file
    // this tracks the current position in each derives list
    // a few options:
    // 1. preprocess the AST to merge all rules sharing the same lhs
    //      a. or make the parser do this up front
    // 2. store these pointers in the packed symbol list
    int **rps = calloc(stats.nonterms, sizeof *rps);

    if (!rps) goto oom;

    // pack the symbols
    if (!pack_symbols(symbols, symtab, stats, rps))
        goto oom;

    // count the number of pattern terms to compute offsets
    // when traversing the AST
    int pterms = 0;

    // pack the pattern definitions
    struct regex_pattern *pat = patterns;
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

        if (!alloc_pattern(pat, sym, pdef->id, pdef->regex))
            goto oom;

        // FIXME: same as above
        strip_quotes(pat->pattern);

        pat++;
    }

    int rn = 1; // rule number, for nonterm rules in symbols (rule #0 reserved)
    int **r = &rules[1];
    struct gram_rule *rule = spec->prules;
    for (; rule; rule = rule->next) {
        // for adding rules to the non-terminal's "derives" list (rules)
        struct gram_symbol *ntsym = htlookup(rule->id, symtab);
        int *rp = rps[ntsym->num];

        struct gram_alt *alt = rule->alts;

        if (!alt || is_empty_rhs(alt->rhses)) {
            if ((*r++ = calloc(1, sizeof *r)) == NULL)
                goto oom;
            *rp++ = rn;
            continue;
        }

        for (; alt; rn++, alt = alt->next) {
            *rp++ = rn;
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

    free(rps);

    free_gram_parser_spec(spec);
    *spec = gram_packed_spec(patterns, symbols, rules, stats);

    return true;
oom:
    return oom_error(error, patterns, symbols, rules, rps);
}

static void free_parsed_spec(struct gram_parser_spec *spec) {
    free_gram_pattern_def(spec->pdefs);
    spec->pdefs = NULL;
    free_gram_rule(spec->prules);
    spec->prules = NULL;
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

    struct gram_symbol *sym = &symbols[1];
    while (sym->num) {
        if (sym->rules) free(sym->rules);
        sym++;
    }

    free(symbols);
}

static void free_rules(int **rules) {
    if (!rules) return;
    int **r = &rules[1];
    while (*r) free(*r), r++;
    free(rules);
}

static void free_packed_spec(struct gram_parser_spec *spec) {
    free_patterns(spec->patterns);
    free_symbols(spec->symbols);
    free_rules(spec->rules);
}

void free_gram_parser_spec(struct gram_parser_spec *spec) {
    if (!spec) return;

    switch (spec->type) {
        case GM_PARSED_SPEC:
        case GM_CHECKED_SPEC:
            free_parsed_spec(spec);
            break;
        case GM_PACKED_SPEC:
            free_packed_spec(spec);
            break;
    }
}

#define PATTERN_DEF_FMT "%s %s\n"
static void print_gram_pattern_def(FILE *handle, struct gram_pattern_def *def) {
    for (; def; def = def->next) {
        if (def->tag_only) fprintf(handle, "@");
        if (def->skip) fprintf(handle, "-");
        fprintf(handle, PATTERN_DEF_FMT, def->id, def->regex);
    }
}

#define RHS_FMT "%s"
#define EMPTY_RHS_FMT "$empty"
#define RHS_SEP_FMT " "
static void _print_gram_rhs(FILE *handle, struct gram_rhs *rhs) {
    switch (rhs->type) {
        case GM_ID_RHS:
        case GM_CHAR_RHS:
        case GM_STRING_RHS:
            fprintf(handle, RHS_FMT, rhs->str);
            break;
        case GM_EMPTY_RHS:
            fprintf(handle, EMPTY_RHS_FMT);
            break;
    }
}

static void print_gram_rhs(FILE *handle, struct gram_rhs *rhs) {
    if (rhs) {
        _print_gram_rhs(handle, rhs);
        for (rhs = rhs->next; rhs; rhs = rhs->next) {
            fprintf(handle, RHS_SEP_FMT);
            _print_gram_rhs(handle, rhs);
        }
    }
}

#define ALT_FMT " | "
static void print_gram_alt(FILE *handle, struct gram_alt *alt) {
    if (alt) {
        print_gram_rhs(handle, alt->rhses);

        for (alt = alt->next; alt; alt = alt->next) {
            fprintf(handle, ALT_FMT);
            print_gram_rhs(handle, alt->rhses);
        }
    }
}

#define RULE_FMT "%s = "
#define RULE_END_FMT ";\n"
static void print_gram_rule(FILE *handle, struct gram_rule *rule) {
    for (; rule; rule = rule->next) {
        fprintf(handle, RULE_FMT, rule->id);
        print_gram_alt(handle, rule->alts);
        fprintf(handle, RULE_END_FMT);
    }
}

static void print_parsed_spec(FILE *handle, struct gram_parser_spec const *spec) {
    invariants(assert_gram_parsed_spec, spec);

    print_gram_pattern_def(handle, spec->pdefs);
    if (spec->prules) {
        fprintf(handle, "---\n");
        print_gram_rule(handle, spec->prules);
    }
}

static void print_packed_spec(FILE *handle, struct gram_parser_spec const *spec) {
    invariants(assert_gram_packed_spec, spec);

    // print packed patterns
    struct regex_pattern *pat = spec->patterns;
    fprintf(handle, "patterns:\n");
    if (pat->sym) {
        fprintf(handle, "  %4s  %s\n", "num", "pattern");
        while (pat->sym) {
            fprintf(handle, "  %4d  %s\n", pat->sym, pat->pattern);
            pat++;
        }
    }
    fprintf(handle, "\n");

    // print packed symbols
    struct gram_symbol *sym = &spec->symbols[1];
    fprintf(handle, "symbols:\n");
    fprintf(handle, "  %4s  %-7s  %s\n", "num", "type", "rules");
    fprintf(handle, "  %4d  ---\n", 0);
    while (sym->num) {
        char *type = sym->type == GM_TERM ? "term" : "nonterm";
        fprintf(handle, "  %4d  %-7s", sym->num, type);
        if (sym->rules) {
            int *r = sym->rules;
            fprintf(handle, "  %d", *r++);
            while (*r)
                fprintf(handle, ", %d", *r), r++;
        }
        fprintf(handle, "\n");
        sym++;
    }
    fprintf(handle, "\n");

    // print packed rules
    int **rule = &spec->rules[1];
    fprintf(handle, "rules:\n");
    fprintf(handle, "  %4s  %s\n", "rule", "symbols");
    fprintf(handle, "  %4d  %s\n", 0, "---");
    int r = 1;
    while (*rule) {
        int *s = *rule;

        fprintf(handle, "  %4d", r);

        if (*s) {
            fprintf(handle, "  %d", *s++);
            while (*s)
                fprintf(handle, ", %d", *s), s++;
        }

        fprintf(handle, "\n");

        rule++;
        r++;
    }
    fprintf(handle, "\n");
}

void print_gram_parser_spec(FILE *handle, struct gram_parser_spec const *spec) {
    if (!spec) return;

    switch (spec->type) {
        case GM_PARSED_SPEC:
        case GM_CHECKED_SPEC:
            print_parsed_spec(handle, spec);
            break;
        case GM_PACKED_SPEC:
            print_packed_spec(handle, spec);
            break;
    }
}

#define OOM_ERROR_FMT_START "| Out of Memory Error\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
void
print_gram_pack_error(FILE *handle, struct gram_pack_error error) {
    switch (error.type) {
        case GM_PACK_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
    }
}

#include "ast.c"
