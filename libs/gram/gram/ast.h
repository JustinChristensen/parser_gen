#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdlib.h>
#include <stdio.h>

struct parser_spec {
    struct pattern_def *pattern_defs;
    struct rule *rules;
};

struct pattern_def {
    char *id;
    char *regex;
    struct pattern_def *next;
};

struct rule {
    char *id;
    struct alt *alts;
    struct rule *next;
};

struct alt {
    struct rhs *rhses;
    struct alt *next;
};

enum rhs_type {
    ID_RHS,
    LIT_RHS,
    EMPTY_RHS
};

struct rhs {
    enum rhs_type type;
    union {
        char *sym;
    };
    struct rhs *next;
};

struct gram_ast_context {
    void *ast;
};

union gram_result ast_to_result(struct gram_ast_context *context);
struct parser_spec *get_parser_spec(struct gram_ast_context *context);

struct parser_spec parser_spec(struct pattern_def *defs, struct rule *rules);
struct pattern_def pattern_def(char *id, char *regex, struct pattern_def *next);
struct rule rule(char *id, struct alt *alts, struct rule *next);
struct alt alt(struct rhs *rhses, struct alt *next);

struct parser_spec *init_parser_spec(struct pattern_def *pattern_defs, struct rule *rules);
struct pattern_def *init_pattern_def(char *id, char *regex, struct pattern_def *next);
struct rule *init_rule(char *id, struct alt *alts, struct rule *next);
struct alt *init_alt(struct rhs *rhses, struct alt *next);
struct rhs *init_id_rhs(char *sym, struct rhs *next);
struct rhs *init_lit_rhs(char *sym, struct rhs *next);
struct rhs *init_empty_rhs(struct rhs *next);

void free_parser_spec(struct parser_spec *spec);
void free_pattern_def(struct pattern_def *def);
void free_rule(struct rule *rule);
void free_alt(struct alt *alt);
void free_rhs(struct rhs *rhs);

bool do_parser_spec(union gram_result result, struct gram_ast_context *context);
bool do_pattern_def(union gram_result result, struct gram_ast_context *context);
bool do_append_pattern_def(union gram_result result, struct gram_ast_context *context);
bool do_rule(union gram_result result, struct gram_ast_context *context);
bool do_append_rule(union gram_result result, struct gram_ast_context *context);
bool do_alt(union gram_result result, struct gram_ast_context *context);
bool do_append_alt(union gram_result result, struct gram_ast_context *context);
bool do_id_rhs(union gram_result result, struct gram_ast_context *context);
bool do_lit_rhs(union gram_result result, struct gram_ast_context *context);
bool do_empty_rhs(union gram_result result, struct gram_ast_context *context);
bool do_append_rhs(union gram_result result, struct gram_ast_context *context);
bool do_head(union gram_result result, struct gram_ast_context *context);

extern bool (*gram_ast_actions[GM_NUM_ACTIONS])(union gram_result result, void *context);

#endif // GRAM_AST_H_


