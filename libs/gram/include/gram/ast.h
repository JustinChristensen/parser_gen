#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdlib.h>
#include <stdio.h>

struct gram_parser_spec {
    struct gram_pattern_def *pattern_defs;
    struct gram_rule *rules;
};

struct gram_pattern_def {
    char *id;
    char *regex;
    struct gram_pattern_def *next;
};

struct gram_rule {
    char *id;
    struct gram_alt *alts;
    struct gram_rule *next;
};

struct gram_alt {
    struct gram_rhs *rhses;
    struct gram_alt *next;
};

enum gram_rhs_type {
    GM_ID_RHS,
    GM_LIT_RHS,
    GM_EMPTY_RHS
};

struct gram_rhs {
    enum gram_rhs_type type;
    union {
        char *sym;
    };
    struct gram_rhs *next;
};


struct gram_parser_spec *init_gram_parser_spec(struct gram_pattern_def *defs, struct gram_rule *rules);
struct gram_pattern_def *init_gram_pattern_def(char *id, char *regex, struct gram_pattern_def *next);
struct gram_rule *init_gram_rule(char *id, struct gram_alt *alts, struct gram_rule *next);
struct gram_alt *init_gram_alt(struct gram_rhs *rhses, struct gram_alt *next);
struct gram_rhs *init_id_gram_rhs(char *sym, struct gram_rhs *next);
struct gram_rhs *init_lit_gram_rhs(char *sym, struct gram_rhs *next);
struct gram_rhs *init_empty_gram_rhs(struct gram_rhs *next);

void free_gram_parser_spec(struct gram_parser_spec *spec);
void free_gram_pattern_def(struct gram_pattern_def *def);
void free_gram_rule(struct gram_rule *rule);
void free_gram_alt(struct gram_alt *alt);
void free_gram_rhs(struct gram_rhs *rhs);

#endif // GRAM_AST_H_


