#include <stdlib.h>
#include <string.h>
#include <base/array.h>
#include "gram/ast.h"
#include "parser.h"

static struct gram_parser_spec *init_parser_spec(struct gram_pattern_def *defs, struct gram_rule *rules, struct array *allocs) {
    struct gram_parser_spec *ps = malloc(sizeof *ps);

    if (!ps) return NULL;

    *ps = (struct gram_parser_spec) { defs, rules };

    apush(&ps, allocs);

    return ps;
}

static struct gram_pattern_def *init_pattern_def(char *id, char *regex, struct gram_pattern_def *next, struct array *allocs) {
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

    *pdef = (struct gram_pattern_def) { id, regex, next };

    apush(&id, allocs);
    apush(&regex, allocs);
    apush(&pdef, allocs);

    return pdef;
}

static struct gram_rule *init_rule(char *id, struct gram_alt *alts, struct gram_rule *next, struct array *allocs) {
    struct gram_rule *rule = malloc(sizeof *rule);

    if (!rule) return NULL;

    if (id && !(id = strdup(id))) {
        free(rule);
        return NULL;
    }

    *rule = (struct gram_rule) { id, alts, next };

    apush(&id, allocs);
    apush(&rule, allocs);

    return rule;
}

static struct gram_alt *init_alt(struct gram_rhs *rhses, struct gram_alt *next, struct array *allocs) {
    struct gram_alt *alt = malloc(sizeof *alt);

    if (!alt) return NULL;

    *alt = (struct gram_alt) { rhses, next };

    apush(&alt, allocs);

    return alt;
}

static struct gram_rhs *init_rhs(enum gram_rhs_type type, char *sym, struct gram_rhs *next, struct array *allocs) {
    struct gram_rhs *rhs = malloc(sizeof *rhs);

    if (!rhs) return NULL;

    if (sym && !(sym = strdup(sym))) {
        free(rhs);
        return NULL;
    }

    *rhs = (struct gram_rhs) { type, .sym = sym, next };

    if (sym) apush(&sym, allocs);
    apush(&rhs, allocs);

    return rhs;
}

static struct gram_rhs *init_id_rhs(char *sym, struct gram_rhs *next, struct array *allocs) {
    return init_rhs(GM_ID_RHS, sym, next, allocs);
}

static struct gram_rhs *init_char_rhs(char *sym, struct gram_rhs *next, struct array *allocs) {
    return init_rhs(GM_CHAR_RHS, sym, next, allocs);
}

static struct gram_rhs *init_string_rhs(char *sym, struct gram_rhs *next, struct array *allocs) {
    return init_rhs(GM_STRING_RHS, sym, next, allocs);
}

static struct gram_rhs *init_empty_rhs(struct gram_rhs *next, struct array *allocs) {
    return init_rhs(GM_EMPTY_RHS, NULL, next, allocs);
}

static void *gast(struct gram_parse_context *context) {
    struct gram_ast_context *result = context->result;
    return result->ast;
}

static bool sast(void *ast, struct gram_parse_context *context) {
    struct gram_ast_context *result = context->result;
    result->ast = ast;
    return ast ? true : false;
}

struct gram_parser_spec *gram_parser_spec(struct gram_ast_context *context) {
    return context->ast;
}

union gram_result ast_result(struct gram_parse_context *context) {
    struct gram_ast_context *result = context->result;
    return (union gram_result) { .ast = result->ast };
}

static struct array *allocs(struct gram_parse_context *context) {
    struct gram_ast_context *result = context->result;
    return result->allocs;
}

static bool do_parser_spec(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_parser_spec(val.ast, gast(context), allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_append_pattern_def(union gram_result val, struct gram_parse_context *context) {
    struct gram_pattern_def *pdef = val.ast;
    pdef->next = gast(context);
    return true;
}

static bool do_pattern_def(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_pattern_def(val.pdef.id, val.pdef.regex, NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_append_rule(union gram_result val, struct gram_parse_context *context) {
    struct gram_rule *rule = val.ast;
    rule->next = gast(context);
    return true;
}

static bool do_rule(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_rule(val.id, gast(context), NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_append_alt(union gram_result val, struct gram_parse_context *context) {
    struct gram_alt *alt = val.ast;
    alt->next = gast(context);
    return true;
}

static bool do_alt(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_alt(val.ast, NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_append_rhs(union gram_result val, struct gram_parse_context *context) {
    struct gram_rhs *rhs = val.ast;
    rhs->next = gast(context);
    return true;
}

static bool do_id_rhs(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_id_rhs(val.id, NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_char_rhs(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_char_rhs(val.lit, NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_string_rhs(union gram_result val, struct gram_parse_context *context) {
    if (sast(init_string_rhs(val.lit, NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_empty_rhs(union gram_result _, struct gram_parse_context *context) {
    if (sast(init_empty_rhs(NULL, allocs(context)), context))
        return true;

    return set_oom_error(context);
}

static bool do_head(union gram_result val, struct gram_parse_context *context)
    { sast(val.ast, context); return true; }

static bool (*const ast_actions[])(union gram_result val, struct gram_parse_context *context) = {
    [AI(GM_DO_PARSER_SPEC)]        = do_parser_spec,
    [AI(GM_DO_PATTERN_DEFS_HEAD)]  = do_head,
    [AI(GM_DO_APPEND_PATTERN_DEF)] = do_append_pattern_def,
    [AI(GM_DO_PATTERN_DEF)]        = do_pattern_def,
    [AI(GM_DO_RULES_HEAD)]         = do_head,
    [AI(GM_DO_APPEND_RULE)]        = do_append_rule,
    [AI(GM_DO_RULE)]               = do_rule,
    [AI(GM_DO_ALTS_HEAD)]          = do_head,
    [AI(GM_DO_APPEND_ALT)]         = do_append_alt,
    [AI(GM_DO_ALT)]                = do_alt,
    [AI(GM_DO_APPEND_RHS)]         = do_append_rhs,
    [AI(GM_DO_ID_RHS)]             = do_id_rhs,
    [AI(GM_DO_CHAR_RHS)]           = do_char_rhs,
    [AI(GM_DO_STRING_RHS)]         = do_string_rhs,
    [AI(GM_DO_EMPTY_RHS)]          = do_empty_rhs
};

bool gram_ast_context(struct gram_ast_context *context) {
    struct array *allocs = init_array(sizeof (void *), 11, 0, 0);

    if (!allocs) return false;

    *context = (struct gram_ast_context) {
        .allocs = allocs
    };

    return true;
}

void free_gram_ast_context(struct gram_ast_context *context) {
    struct array *allocs = context->allocs;

    while (!aempty(allocs)) {
        void *p;
        apop(&p, allocs);
        free(p);
    }

    free_array(allocs);
    context->allocs = NULL;
    context->ast = NULL;
}

struct gram_result_interface gram_ast_iface = {
    .actions = ast_actions,
    .result = ast_result
};


