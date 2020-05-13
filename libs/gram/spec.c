#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <base/debug.h>
#include "gram/spec.h"

#include "internal/assert.c"

static struct gram_stats nsymbols(struct gram_stats stats) {
    stats.symbols = stats.terms + stats.nonterms;
    return stats;
}

struct gram_parser_spec gram_parsed_spec(struct gram_pattern_def *pdefs, struct gram_rule *rules, struct gram_stats stats) {
    return (struct gram_parser_spec) {
        GM_PARSED_SPEC,
        .pdefs = pdefs, .prules = rules, .stats = nsymbols(stats)
    };
}

struct gram_parser_spec gram_packed_spec(
    struct regex_pattern *patterns, struct gram_symbol *symbols,
    gram_sym_no **rules, struct gram_stats stats
) {
    return (struct gram_parser_spec) {
        GM_PACKED_SPEC,
        .patterns = patterns, .symbols = symbols, .rules = rules,
        .stats = nsymbols(stats)
    };
}

#define N_(next) (1 + (next ? next->n : 0))

struct gram_pattern_def *init_gram_pattern_def(
    struct regex_loc loc,
    char *id, char *regex, bool skip,
    struct gram_pattern_def *next
) {
    struct gram_pattern_def *pdef = malloc(sizeof *pdef);

    if (!pdef) return NULL;

    if (id && ((id = strdup(id)) == NULL)) {
        free(pdef);
        return NULL;
    }

    if (regex && ((regex = strdup(regex)) == NULL)) {
        free(pdef);
        free(id);
        return NULL;
    }

    *pdef = (struct gram_pattern_def) {
        loc, id, regex,
        .skip = skip,
        next, N_(next)
    };

    return pdef;
}

struct gram_rule *init_gram_rule(struct regex_loc loc, char *id, struct gram_alt *alts, struct gram_rule *next) {
    struct gram_rule *rule = malloc(sizeof *rule);

    if (!rule) return NULL;

    if (id && ((id = strdup(id)) == NULL)) {
        free(rule);
        return NULL;
    }

    *rule = (struct gram_rule) { loc, id, alts, next, N_(next) };

    return rule;
}

struct gram_alt *init_gram_alt(struct regex_loc loc, struct gram_rhs *rhses, struct gram_alt *next) {
    struct gram_alt *alt = malloc(sizeof *alt);

    if (!alt) return NULL;

    *alt = (struct gram_alt) { loc, rhses, next, N_(next) };

    return alt;
}

static struct gram_rhs *init_rhs(struct regex_loc loc, enum gram_rhs_type type, char *str, struct gram_rhs *next) {
    struct gram_rhs *rhs = malloc(sizeof *rhs);

    if (!rhs) return NULL;

    if (str && ((str = strdup(str)) == NULL)) {
        free(rhs);
        return NULL;
    }

    *rhs = (struct gram_rhs) { loc, type, .str = str, next, N_(next) };

    return rhs;
}

struct gram_rhs *init_id_gram_rhs(struct regex_loc loc, char *str, struct gram_rhs *next) {
    return init_rhs(loc, GM_ID_RHS, str, next);
}

struct gram_rhs *init_string_gram_rhs(struct regex_loc loc, char *str, struct gram_rhs *next) {
    return init_rhs(loc, GM_STRING_RHS, str, next);
}

struct gram_rhs *init_empty_gram_rhs(struct regex_loc loc, struct gram_rhs *next) {
    return init_rhs(loc, GM_EMPTY_RHS, NULL, next);
}

void free_gram_pattern_def(struct gram_pattern_def *pdef) {
    if (!pdef) return;

    for (struct gram_pattern_def *next; pdef; pdef = next) {
        next = pdef->next;
        free(pdef->id);
        free(pdef->regex);
        free(pdef);
    }
}

void free_gram_rule(struct gram_rule *rule) {
    if (!rule) return;

    for (struct gram_rule *next; rule; rule = next) {
        next = rule->next;
        free(rule->id);
        free_gram_alt(rule->alts);
        free(rule);
    }
}

void free_gram_alt(struct gram_alt *alt) {
    if (!alt) return;

    for (struct gram_alt *next; alt; alt = next) {
        next = alt->next;
        free_gram_rhs(alt->rhses);
        free(alt);
    }
}

void free_gram_rhs(struct gram_rhs *rhs) {
    if (!rhs) return;

    for (struct gram_rhs *next; rhs; rhs = next) {
        next = rhs->next;
        switch (rhs->type) {
            case GM_ID_RHS:
            case GM_STRING_RHS:
                free(rhs->str);
                break;
            case GM_EMPTY_RHS:
                break;
        }
        free(rhs);
    }
}

static void free_parsed_spec(struct gram_parser_spec *spec) {
    free_gram_pattern_def(spec->pdefs);
    spec->pdefs = NULL;
    free_gram_rule(spec->prules);
    spec->prules = NULL;
}

#include "internal/spec.c"

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

bool gram_rhses_empty(struct gram_rhs *rhses) {
    if (!rhses) return true;
    struct gram_rhs *rhs = rhses;
    bool empty = true;
    do empty = empty && rhs->type == GM_EMPTY_RHS;
    while ((rhs = rhs->next));
    return empty;
}

bool gram_exists(struct gram_parser_spec const *spec) {
    return spec->stats.nonterms > 0;
}

bool gram_symbol_null(struct gram_symbol const *sym) {
    return sym->num == 0;
}

struct gram_symbol *gram_term0(struct gram_parser_spec const *spec) {
    return &spec->symbols[GM_SYMBOL0];
}

struct gram_symbol *gram_nonterm0(struct gram_parser_spec const *spec) {
    return &spec->symbols[spec->stats.terms + 1];
}

struct gram_symbol *gram_symbol0(struct gram_parser_spec const *spec) {
    return gram_term0(spec);
}

gram_sym_no **gram_rule0(struct gram_parser_spec const *spec) {
    return &spec->rules[GM_START];
}

struct gram_symbol *gram_start_sym(struct gram_parser_spec const *spec) {
    return gram_exists(spec) ? gram_nonterm0(spec) : gram_term0(spec);
}

char **gram_symbol_strings(struct gram_parser_spec const *spec) {
    struct gram_stats const stats = spec->stats;
    char **symtab = calloc(offs(stats.symbols), sizeof *symtab);
    if (!symtab) return NULL;

    struct gram_symbol *sym = gram_symbol0(spec);
    while (!gram_symbol_null(sym)) {
        if (sym->str && !(symtab[sym->num] = strdup(sym->str))) {
            free_symtab(symtab, stats);
            return NULL;
        }
        sym++;
    }

    return symtab;
}

#define PATTERN_DEF_FMT "%s %s\n"
static void print_gram_pattern_def(FILE *handle, struct gram_pattern_def *def) {
    for (; def; def = def->next) {
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
    invariant(assert_parsed_spec, spec);

    print_gram_pattern_def(handle, spec->pdefs);
    if (spec->prules) {
        fprintf(handle, "---\n");
        print_gram_rule(handle, spec->prules);
    }
}

#define PATTERNS_TITLE_FMT "patterns:\n"
#define PATTERNS_HEADER_FMT "  %4s  %s\n"
#define SYMBOLS_TITLE_FMT "symbols:\n"
#define SYMBOLS_HEADER_FMT "  %4s  %-7s  %-16s  %s\n"
#define RULES_TITLE_FMT "rules:\n"
#define RULES_HEADER_FMT "  %4s  %s\n"
static void print_packed_spec(FILE *handle, struct gram_parser_spec const *spec) {
    invariant(assert_packed_spec, spec);

    // print packed patterns
    struct regex_pattern *pat = spec->patterns;
    if (!regex_null_pattern(pat)) {
        fprintf(handle, PATTERNS_TITLE_FMT);
        fprintf(handle, PATTERNS_HEADER_FMT, "num", "pattern");
        while (!regex_null_pattern(pat)) {
            fprintf(handle, "  %4d  %s\n", pat->sym, pat->pattern);
            pat++;
        }

        fprintf(handle, "\n");
    }

    // print packed symbols
    struct gram_symbol *sym = gram_symbol0(spec);
    fprintf(handle, SYMBOLS_TITLE_FMT);
    fprintf(handle, SYMBOLS_HEADER_FMT, "num", "type", "sym", "derives");
    if (!gram_symbol_null(sym)) {
        fprintf(handle, "  %4d  %-7s\n", sym->num, "eof");
        sym++;
    }

    while (!gram_symbol_null(sym)) {
        char *type = "nonterm";
        if (sym->type == GM_TERM) type = "term";

        fprintf(handle, "  %4d  %-7s  %-16s", sym->num, type, sym->str);

        if (sym->type == GM_NONTERM) {
            gram_rule_no *r = sym->derives;
            fprintf(handle, "  %d", *r++);
            while (*r)
                fprintf(handle, " %d", *r), r++;
        }

        fprintf(handle, "\n");
        sym++;
    }
    fprintf(handle, "\n");

    // print packed rules
    gram_sym_no **rule = gram_rule0(spec);
    fprintf(handle, RULES_TITLE_FMT);
    fprintf(handle, RULES_HEADER_FMT, "rule", "symbols");
    int r = 1;
    while (*rule) {
        gram_sym_no *s = *rule;

        fprintf(handle, "  %4d", r);

        if (*s) {
            fprintf(handle, "  %d", *s++);
            while (*s)
                fprintf(handle, " %d", *s), s++;
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

