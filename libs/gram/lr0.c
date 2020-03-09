#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <base/hash_table.h>
#include <base/array.h>
#include "gram/lr0.h"

static struct lr0_error scanner_error(struct regex_error error) {
    return (struct lr0_error) { .type = LR0_SCANNER_ERROR, .scanerr = error };
}

static bool set_scanner_error(struct regex_error error, struct lr0_context *context) {
    if (!context->has_error) {
        context->has_error = true;
        context->error = scanner_error(error);
    }

    return false;
}

static struct lr0_error oom_error(struct regex_error error) {
    return (struct lr0_error) { .type = LR0_OOM_ERROR };
}

static bool set_oom_error(struct regex_error error, struct lr0_context *context) {
    if (!context->has_error) {
        context->has_error = true;
        context->error = oom_error(error);
    }

    return false;
}

#define INIT_STRINGS 11
#define INIT_RULES 7
bool lr0_context(
    struct lr0_context *context,
    struct regex_pattern const *patterns
) {
    struct nfa_context scanner = { 0 };

    if (!nfa_context(&scanner, RX_PATTERNS {
        RX_ALPHA_(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY), RX_SPACE(RX_TAG_ONLY)
        RX_END_PATTERNS
    })) {
        set_scanner_error(nfa_error(&scanner), context);
        free_nfa_context(&scanner);
        return false;
    }

    if (!nfa_add_patterns(patterns, &scanner)) {
        set_scanner_error(nfa_error(&scanner), context);
        free_nfa_context(&scanner);
        return false;
    }

    struct hash_table *symtab = NULL;
    if ((symtab = init_hash_table(NULL)) == NULL) {
        free_nfa_context(&scanner);
        return set_oom_error(context);
    }

    struct array *strings = NULL;
    if ((strings = init_array(sizeof (char *), INIT_STRINGS, 0, 0)) == NULL) {
        free_nfa_context(&scanner);
        free_hash_table(symtab);
        return set_oom_error(context);
    }

    *context = (struct lr0_context) {
        .nsyms = 0,
        .strings = strings,
        .symtab = symtab,
        .scanner = scanner
    };

    return true;
}
#undef INIT_STRINGS
#undef INIT_RULES

void free_lr0_context(struct lr0_context *context) {
    free_array(context->strings);
    free_hash_table(context->symtab);
    free_nfa_context(&context->scanner);
    free_array(context->rules);
    *context = (struct lr0_context) { 0 };
}

static bool do_parser_spec(union gram_result val, struct gram_parse_context *context) {
}

static bool do_append_pattern_def(union gram_result val, struct gram_parse_context *context) {
}

static bool do_pattern_def(union gram_result val, struct gram_parse_context *context) {
    int sym;

    if (val.pdef.tag_only) {
        sym = RX_TAG_ONLY;
    } else if (val.pdef.skip) {
        sym = RX_SKIP;
    } else {
        sym = context->nsyms++;
    }

    if (!nfa_regex(sym, val.pdef.id, val.pdef.regex))
        return set_scanner_error(nfa_error(&context->scanner), context);

    struct hash_table *symtab = context->symtab;

    if (sym > 0) htinsert_i(val.pdef.id, sym, context);

    return true;
}

static bool do_append_rule(union gram_result val, struct gram_parse_context *context) {
}

static bool do_rule(union gram_result val, struct gram_parse_context *context) {
}

static bool do_append_alt(union gram_result val, struct gram_parse_context *context) {
}

static bool do_alt(union gram_result val, struct gram_parse_context *context) {
}

static bool do_append_rhs(union gram_result val, struct gram_parse_context *context) {
}

static bool do_id_rhs(union gram_result val, struct gram_parse_context *context) {
}

static bool do_char_rhs(union gram_result val, struct gram_parse_context *context) {
}

static bool do_string_rhs(union gram_result val, struct gram_parse_context *context) {
}

static bool do_empty_rhs(union gram_result _, struct gram_parse_context *context) {
}

static bool do_head(union gram_result val, struct gram_parse_context *context) {
}

static bool (*const lr0_actions[])(union gram_result val, struct gram_parse_context *context) = {
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

