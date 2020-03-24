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
        return set_oom_error(__FILE__, __LINE__, context);
    }

    struct array *strings = NULL;
    if ((strings = init_array(sizeof (char *), INIT_STRINGS, 0, 0)) == NULL) {
        free_nfa_context(&scanner);
        free_hash_table(symtab);
        return set_oom_error(__FILE__, __LINE__, context);
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
