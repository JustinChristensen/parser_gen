#ifndef GRAM_AST_C_
#define GRAM_AST_C_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "ast.h"

#define N_(next) (1 + (next ? next->n : 0))

struct gram_pattern_def *init_gram_pattern_def(
    char *id, char *regex,
    bool tag_only, bool skip,
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
        id, regex,
        .tag_only = tag_only,
        .skip = skip,
        next, N_(next)
    };

    return pdef;
}

struct gram_rule *init_gram_rule(char *id, struct gram_alt *alts, struct gram_rule *next) {
    struct gram_rule *rule = malloc(sizeof *rule);

    if (!rule) return NULL;

    if (id && ((id = strdup(id)) == NULL)) {
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

static struct gram_rhs *init_rhs(enum gram_rhs_type type, char *str, struct gram_rhs *next) {
    struct gram_rhs *rhs = malloc(sizeof *rhs);

    if (!rhs) return NULL;

    if (str && ((str = strdup(str)) == NULL)) {
        free(rhs);
        return NULL;
    }

    *rhs = (struct gram_rhs) { type, .str = str, next, N_(next) };

    return rhs;
}

struct gram_rhs *init_id_gram_rhs(char *str, struct gram_rhs *next) {
    return init_rhs(GM_ID_RHS, str, next);
}

struct gram_rhs *init_char_gram_rhs(char *str, struct gram_rhs *next) {
    return init_rhs(GM_CHAR_RHS, str, next);
}

struct gram_rhs *init_string_gram_rhs(char *str, struct gram_rhs *next) {
    return init_rhs(GM_STRING_RHS, str, next);
}

struct gram_rhs *init_empty_gram_rhs(struct gram_rhs *next) {
    return init_rhs(GM_EMPTY_RHS, NULL, next);
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
            case GM_CHAR_RHS:
            case GM_STRING_RHS:
                free(rhs->str);
                break;
            case GM_EMPTY_RHS:
                break;
        }
        free(rhs);
    }
}

#endif // GRAM_AST_C_
