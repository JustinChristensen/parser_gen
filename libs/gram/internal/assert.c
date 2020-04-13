#ifndef GRAM_ASSERT_C_
#define GRAM_ASSERT_C_ 1

#include <assert.h>
#include <base/assert.h>
#include "gram/parser.h"

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

#ifdef INVARIANTS

INVARIANT(assert_parsed_spec, struct gram_parser_spec const *spec) {
    check(spec != NULL);
    check(spec->type == GM_PARSED_SPEC);
}

INVARIANT(assert_packed_spec, struct gram_parser_spec const *spec) {
    check(spec != NULL);
    check(spec->type == GM_PACKED_SPEC);

    if (spec->symbols) {
        check(spec->symbols[0].num == 0);
        check(spec->symbols[0].derives == NULL);
    }

    if (spec->rules)
        check(spec->rules[0] == NULL);
}

INVARIANT(assert_symbol_index, unsigned int i, struct gram_parser_spec const *spec) {
    check(i >= 1 && i <= spec->stats.symbols);
}

INVARIANT(assert_rule_index, unsigned int i, struct gram_parser_spec const *spec) {
    check(i >= 1 && i <= spec->stats.symbols);
}

#endif

#pragma clang diagnostic pop

#endif // GRAM_ASSERT_C_
