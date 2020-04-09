#ifndef GRAM_ASSERT_C_
#define GRAM_ASSERT_C_ 1

#include <assert.h>
#include <base/debug.h>
#include "gram/parser.h"

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

#ifdef INVARIANTS
static void assert_parsed_spec(struct gram_parser_spec const *spec) {
    assert(spec != NULL);
    assert(spec->type == GM_PARSED_SPEC);
}

static void assert_packed_spec(struct gram_parser_spec const *spec) {
    assert(spec != NULL);
    assert(spec->type == GM_PACKED_SPEC);

    assert(spec->symbols != NULL);
    assert(spec->symbols[0].num == 0);
    assert(spec->symbols[0].derives == NULL);

    assert(spec->rules != NULL);
    assert(spec->rules[0] == NULL);
}

static void assert_symbol_index(unsigned int i, struct gram_parser_spec const *spec) {
    assert(i >= 1 && i <= spec->stats.symbols);
}

static void assert_rule_index(unsigned int i, struct gram_parser_spec const *spec) {
    assert(i >= 1 && i <= spec->stats.symbols);
}
#endif

#pragma clang diagnostic pop

#endif // GRAM_ASSERT_C_
