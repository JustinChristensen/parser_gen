#include <base/array.h>
#include "gram/parser.h"
#include "gram/ast.h"

#define ACTION (void (*)(union gram_result, void *))
void (*gram_ast_actions[])(union gram_result result, void *context) = {
    [AI(DO_PARSER_SPEC)] = ACTION do_parser_spec,

    [AI(DO_PATTERN_DEF)] = ACTION do_pattern_def,
    [AI(DO_APPEND_PATTERN_DEF)] = ACTION do_append_pattern_def,
    [AI(DO_PATTERN_DEFS_HEAD)] = ACTION do_head,

    [AI(DO_RULE)] = ACTION do_rule,
    [AI(DO_APPEND_RULE)] = ACTION do_append_rule,
    [AI(DO_RULES_HEAD)] = ACTION do_head,

    [AI(DO_ALT)] = ACTION do_alt,
    [AI(DO_APPEND_ALT)] = ACTION do_append_alt,
    [AI(DO_ALTS_HEAD)] = ACTION do_head,

    [AI(DO_ID_RHS)] = ACTION do_id_rhs,
    [AI(DO_LIT_RHS)] = ACTION do_lit_rhs,
    [AI(DO_EMPTY_RHS)] = ACTION do_empty_rhs,
    [AI(DO_APPEND_RHS)] = ACTION do_append_rhs,
    [AI(DO_RHSES_HEAD)] = ACTION do_head
};
#undef ACTION

struct gram_ast_context gram_ast_context() {
    struct array *nodes = init_array(sizeof (union ast_type), AST_START_SIZE, 0, 0);
    return (struct gram_ast_context) { NULL, nodes };
}

void free_ast_context(struct gram_ast_context *context) {
    free_array(context->nodes);
}

static void set_ast(void *ast, struct gram_ast_context *context) {
    context->ast = ast;
}

struct parser_spec parser_spec(struct pattern_def *pattern_defs, struct rule *rules) {
    return (struct parser_spec) { pattern_defs, rules };
}

struct pattern_def pattern_def(char *id, char *regex, struct pattern_def *next) {
    return (struct pattern_def) { id, regex, next };
}

struct rule rule(char *id, struct alt *alts, struct rule *next) {
    return (struct rule) { id, alts, next };
}

struct alt alt(struct rhs *rhses, struct alt *next) {
    return (struct alt) { rhses, next };
}

struct rhs id_rhs(char *sym, struct rhs *next) {
    return (struct rhs) { ID_RHS, sym, next };
}

struct rhs lit_rhs(char *sym, struct rhs *next) {
    return (struct rhs) { LIT_RHS, sym, next };
}

struct rhs empty_rhs(struct rhs *next) {
    return (struct rhs) { EMPTY_RHS, NULL, next };
}

void do_parser_spec(union gram_result result, struct gram_ast_context *context) {
}

void do_pattern_def(union gram_result result, struct gram_ast_context *context) {
}

void do_append_pattern_def(union gram_result result, struct gram_ast_context *context) {
}

void do_rule(union gram_result result, struct gram_ast_context *context) {
}

void do_append_rule(union gram_result result, struct gram_ast_context *context) {
}

void do_alt(union gram_result result, struct gram_ast_context *context) {
}

void do_append_alt(union gram_result result, struct gram_ast_context *context) {
}

void do_id_rhs(union gram_result result, struct gram_ast_context *context) {
}

void do_lit_rhs(union gram_result result, struct gram_ast_context *context) {
}

void do_empty_rhs(union gram_result result, struct gram_ast_context *context) {
}

void do_append_rhs(union gram_result result, struct gram_ast_context *context) {o
}

void do_head(union gram_result result, struct gram_ast_context *context) {
    set_ast(result.ast, context);
}

