#ifndef GRAM_PARSER_IH_
#define GRAM_PARSER_IH_ 1

#include "gram/parser.h"

#define NUM_TERMINALS ((GM_WHITESPACE_T + 1) - GM_ERROR)
#define NTEND (GM_RHS_NT + 1)
#define NUM_NONTERMINALS (NTEND - NUM_TERMINALS)
#define NUM_ACTIONS ((GM_DO_EMPTY_RHS + 1) - NTEND)
#define AI(sym) (sym - NTEND)

static union gram_result const NULL_RESULT = { 0 };

static struct gram_error oom_error() {
    return (struct gram_error) { .type = GM_OOM_ERROR };
}

static bool set_oom_error(struct gram_parse_context *context) {
    if (!context->has_error) {
        context->has_error = true;
        context->error = oom_error();
    }

    return false;
}

#endif // GRAM_PARSER_IH_
