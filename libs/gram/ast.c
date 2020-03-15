#include <stdlib.h>
#include <string.h>
#include <base/array.h>
#include "gram/ast.h"
#include "parser.h"

#define N_(next) (1 + (next ? next->n : 0))

struct gram_parser_spec *init_gram_parser_spec(struct gram_pattern_def *pdefs, struct gram_rule *rules) {
    struct gram_parser_spec *ps = malloc(sizeof *ps);

    if (!ps) return NULL;

    *ps = (struct gram_parser_spec) { pdefs, rules };

    return ps;
}

struct gram_pattern_def *init_gram_pattern_def(char *id, char *regex, struct gram_pattern_def *next) {
    struct gram_pattern_def *pdef = malloc(sizeof *pdef);

    if (!pdef) return NULL;

    if (id && !(id = strdup(id))) {
        free(pdef);
        return NULL;
    }

    if (regex && !(regex = strdup(regex))) {
        free(pdef);
        free(id);
        return NULL;
    }

    *pdef = (struct gram_pattern_def) { id, regex, next, N_(next) };

    return pdef;
}

struct gram_rule *init_gram_rule(char *id, struct gram_alt *alts, struct gram_rule *next) {
    struct gram_rule *rule = malloc(sizeof *rule);

    if (!rule) return NULL;

    if (id && !(id = strdup(id))) {
        free(rule);
        return NULL;
    }

    *rule = (struct gram_rule) { id, alts, next, N_(next) };

    return rule;
}

struct gram_alt *init_gram_alt(struct gram_rhs *rhses, struct gram_alt *next) {
    struct gram_alt *alt = malloc(sizeof *alt);

    if (!alt) return NULL;

    *alt = (struct gram_alt) { rhses, next, N_(next) };

    return alt;
}

static struct gram_rhs *init_rhs(enum gram_rhs_type type, char *sym, struct gram_rhs *next) {
    struct gram_rhs *rhs = malloc(sizeof *rhs);

    if (!rhs) return NULL;

    if (sym && !(sym = strdup(sym))) {
        free(rhs);
        return NULL;
    }

    *rhs = (struct gram_rhs) { type, .sym = sym, next, N_(next) };

    return rhs;
}

struct gram_rhs *init_id_gram_rhs(char *sym, struct gram_rhs *next) {
    return init_rhs(GM_ID_RHS, sym, next);
}

struct gram_rhs *init_char_gram_rhs(char *sym, struct gram_rhs *next) {
    return init_rhs(GM_CHAR_RHS, sym, next);
}

struct gram_rhs *init_string_gram_rhs(char *sym, struct gram_rhs *next) {
    return init_rhs(GM_STRING_RHS, sym, next);
}

struct gram_rhs *init_empty_gram_rhs(struct gram_rhs *next) {
    return init_rhs(GM_EMPTY_RHS, NULL, next);
}

void free_gram_parser_spec(struct gram_parser_spec *spec) {
    free_gram_pattern_def(spec->pdefs);
    spec->pdefs = NULL;
    free_gram_rule(spec->rules);
    spec->rules = NULL;
}

void free_gram_pattern_def(struct gram_pattern_def *pdef) {
    for (struct gram_pattern_def *next; pdef; pdef = next) {
        next = pdef->next;
        free(pdef->id);
        free(pdef->regex);
        free(pdef);
    }
}

void free_gram_rule(struct gram_rule *rule) {
    for (struct gram_rule *next; rule; rule = next) {
        next = rule->next;
        free(rule->id);
        free_gram_alt(rule->alts);
        free(rule);
    }
}

void free_gram_alt(struct gram_alt *alt) {
    for (struct gram_alt *next; alt; alt = next) {
        next = alt->next;
        free_gram_rhs(alt->rhses);
        free(alt);
    }
}

void free_gram_rhs(struct gram_rhs *rhs) {
    for (struct gram_rhs *next; rhs; rhs = next) {
        next = rhs->next;
        switch (rhs->type) {
            case GM_ID_RHS:
            case GM_CHAR_RHS:
            case GM_STRING_RHS:
                free(rhs->sym);
                break;
            case GM_EMPTY_RHS:
                break;
        }
        free(rhs);
    }
}

