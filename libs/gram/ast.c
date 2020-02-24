#include <stdlib.h>
#include <assert.h>
#include "parser.h"
#include "gram/ast.h"

static void *gast(struct gram_ast_context *context) { return context->ast; }
static void sast(void *ast, struct gram_ast_context *context)  { context->ast = ast;  }

static struct gram_parser_spec parser_spec(struct gram_pattern_def *pattern_defs, struct gram_rule *rules) {
    return (struct gram_parser_spec) { pattern_defs, rules };
}

static struct gram_pattern_def pattern_def(char *id, char *regex, struct gram_pattern_def *next) {
    return (struct gram_pattern_def) { id, regex, next };
}

static struct gram_rule rule(char *id, struct gram_alt *alts, struct gram_rule *next) {
    return (struct gram_rule) { id, alts, next };
}

static struct gram_alt alt(struct gram_rhs *rhses, struct gram_alt *next) {
    return (struct gram_alt) { rhses, next };
}

static struct gram_rhs rhs(enum gram_rhs_type type, char *sym, struct gram_rhs *next) {
    return (struct gram_rhs) { type, .sym = sym, next };
}

static struct gram_parser_spec *init_parser_spec(struct gram_pattern_def *defs, struct gram_rule *rules) {
    struct gram_parser_spec *n = malloc(sizeof *n);
    assert(n != NULL);
    *n = parser_spec(defs, rules);
    return n;
}

static struct gram_pattern_def *init_pattern_def(char *id, char *regex, struct gram_pattern_def *next) {
    struct gram_pattern_def *n = malloc(sizeof *n);
    assert(n != NULL);
    *n = pattern_def(id, regex, next);
    return n;
}

static struct gram_rule *init_rule(char *id, struct gram_alt *alts, struct gram_rule *next) {
    struct gram_rule *n = malloc(sizeof *n);
    assert(n != NULL);
    *n = rule(id, alts, next);
    return n;
}

static struct gram_alt *init_alt(struct gram_rhs *rhses, struct gram_alt *next) {
    struct gram_alt *n = malloc(sizeof *n);
    assert(n != NULL);
    *n = alt(rhses, next);
    return n;
}

static struct gram_rhs *init_rhs(enum gram_rhs_type type, char *sym, struct gram_rhs *next) {
    struct gram_rhs *n = malloc(sizeof *n);
    assert(n != NULL);
    *n = rhs(type, sym, next);
    return n;
}

static struct gram_rhs *init_id_rhs(char *sym, struct gram_rhs *next) {
    return init_rhs(GM_ID_RHS, sym, next);
}

static struct gram_rhs *init_lit_rhs(char *sym, struct gram_rhs *next) {
    return init_rhs(GM_LIT_RHS, sym, next);
}

static struct gram_rhs *init_empty_rhs(struct gram_rhs *next) {
    return init_rhs(GM_EMPTY_RHS, NULL, next);
}

static void free_rhs(struct gram_rhs *rhs) {
    for (struct gram_rhs *next; rhs; rhs = next)
        next = rhs->next, free(rhs);
}

static void free_alt(struct gram_alt *alt) {
    for (struct gram_alt *next; alt; alt = next) {
        next = alt->next;
        free_rhs(alt->rhses);
        alt->rhses = NULL;
        free(alt);
    }
}

static void free_rule(struct gram_rule *rule) {
    for (struct gram_rule *next; rule; rule = next) {
        next = rule->next;
        free_alt(rule->alts);
        rule->alts = NULL;
        free(rule);
    }
}

static void free_pattern_def(struct gram_pattern_def *def) {
    for (struct gram_pattern_def *next; def; def = next)
        next = def->next, free(def);
}

void free_parser_spec(struct gram_parser_spec *spec) {
    free_pattern_def(spec->pattern_defs);
    spec->pattern_defs = NULL;
    free_rule(spec->rules);
    spec->rules = NULL;
    free(spec);
}

static bool do_parser_spec(union gram_result result, struct gram_ast_context *context) {
    sast(init_parser_spec(result.ast, gast(context)), context);
    return true;
}

static bool do_pattern_def(union gram_result result, struct gram_ast_context *context) {
    sast(init_pattern_def(result.pdef.id, result.pdef.regex, NULL), context);
    return true;
}

static bool do_rule(union gram_result result, struct gram_ast_context *context) {
    sast(init_rule(result.id, gast(context), NULL), context);
    return true;
}

static bool do_alt(union gram_result _, struct gram_ast_context *context) {
    sast(init_alt(gast(context), NULL), context);
    return true;
}

static bool do_id_rhs(union gram_result result, struct gram_ast_context *context) {
    sast(init_id_rhs(result.id, NULL), context);
    return true;
}

static bool do_lit_rhs(union gram_result result, struct gram_ast_context *context) {
    sast(init_lit_rhs(result.lit, NULL), context);
    return true;
}

static bool do_empty_rhs(union gram_result _, struct gram_ast_context *context) {
    sast(init_empty_rhs(NULL), context);
    return true;
}

static bool do_head(union gram_result result, struct gram_ast_context *context) {
    sast(result.ast, context);
    return true;
}

static bool do_append_pattern_def(union gram_result result, struct gram_ast_context *context) {
    struct gram_pattern_def *pdef = result.ast;
    pdef->next = gast(context);
    return true;
}

static bool do_append_rule(union gram_result result, struct gram_ast_context *context) {
    struct gram_rule *rule = result.ast;
    rule->next = gast(context);
    return true;
}

static bool do_append_alt(union gram_result result, struct gram_ast_context *context) {
    struct gram_alt *alt = result.ast;
    alt->next = gast(context);
    return true;
}

static bool do_append_rhs(union gram_result result, struct gram_ast_context *context) {
    struct gram_rhs *rhs = result.ast;
    rhs->next = gast(context);
    return true;
}

static bool (*gram_ast_actions[])(union gram_result result, struct gram_ast_context *context) = {
    [AI(GM_DO_PARSER_SPEC)] =        do_parser_spec,

    [AI(GM_DO_PATTERN_DEF)] =        do_pattern_def,
    [AI(GM_DO_APPEND_PATTERN_DEF)] = do_append_pattern_def,
    [AI(GM_DO_PATTERN_DEFS_HEAD)] =  do_head,

    [AI(GM_DO_RULE)] =               do_rule,
    [AI(GM_DO_APPEND_RULE)] =        do_append_rule,
    [AI(GM_DO_RULES_HEAD)] =         do_head,

    [AI(GM_DO_ALT)] =                do_alt,
    [AI(GM_DO_APPEND_ALT)] =         do_append_alt,
    [AI(GM_DO_ALTS_HEAD)] =          do_head,

    [AI(GM_DO_ID_RHS)] =             do_id_rhs,
    [AI(GM_DO_LIT_RHS)] =            do_lit_rhs,
    [AI(GM_DO_EMPTY_RHS)] =          do_empty_rhs,
    [AI(GM_DO_APPEND_RHS)] =         do_append_rhs,
    [AI(GM_DO_RHSES_HEAD)] =         do_head
};

static union gram_result ast_to_result(struct gram_ast_context *context) {
    return (union gram_result) { .ast = gast(context) };
}

struct gram_parser_spec *get_parser_spec(struct gram_ast_context *context) {
    return gast(context);
}

struct gram_parse_interface const gram_ast_parse_iface = {
    .result = RESULTFN ast_to_result,
    .has_error = HASERRFN NULL,
    .error = ERRFN NULL,
    .actions = ACTIONS gram_ast_actions,
};
