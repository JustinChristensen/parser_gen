#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <base/alloc.h>

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
    char *sym;
    struct rhs *next;
};

struct gram_ast_context {
    void *ast;
    struct balloc *alloc;
};

struct gram_ast_context gram_ast_context(void *ast, struct balloc *alloc);
void set_ast(void *ast, struct gram_ast_context *context);

struct parser_spec parser_spec(struct pattern_def *pattern_defs, struct rule *rules);
struct pattern_def pattern_def(char *id, char *regex, struct pattern_def *next);
struct rule rule(char *id, struct alt *alts, struct rule *next);
struct alt alt(struct rhs *rhses, struct alt *next);
struct rhs id_rhs(char *sym, struct rhs *next);
struct rhs lit_rhs(char *sym, struct rhs *next);
struct rhs empty_rhs(struct rhs *next);

void do_parser_spec(union gram_result result, struct gram_ast_context *context);
void do_pattern_def(union gram_result result, struct gram_ast_context *context);
void do_append_pattern_def(union gram_result result, struct gram_ast_context *context);
void do_rule(union gram_result result, struct gram_ast_context *context);
void do_append_rule(union gram_result result, struct gram_ast_context *context);
void do_alt(union gram_result result, struct gram_ast_context *context);
void do_append_alt(union gram_result result, struct gram_ast_context *context);
void do_id_rhs(union gram_result result, struct gram_ast_context *context);
void do_lit_rhs(union gram_result result, struct gram_ast_context *context);
void do_empty_rhs(union gram_result result, struct gram_ast_context *context);
void do_append_rhs(union gram_result result, struct gram_ast_context *context);
void do_head(union gram_result result, struct gram_ast_context *context);

void (**gram_ast_actions)(union gram_result result, void *context);

#endif // GRAM_AST_H_


