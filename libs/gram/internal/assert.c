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

    struct gram_stats stats = spec->stats;

    // try to enforce null-termination
    if (stats.patterns) {
        check(spec->patterns != NULL);
        check(regex_null_pattern(&spec->patterns[stats.patterns]));
    }

    check(stats.symbols > 0);
    check(spec->symbols != NULL);
    check(spec->symbols[0].num == 0);
    check(spec->symbols[0].derives == NULL);
    check(gram_symbol_null(&spec->symbols[stats.symbols + 1]));

    check(stats.rules > 0);
    check(spec->rules != NULL);
    check(spec->rules[0] == NULL);
    check(spec->rules[stats.rules + 1] == NULL);

    if (stats.nonterms) check(spec->rules[1][1] == GM_EOF);
    else                check(spec->rules[1][0] == GM_EOF);
}

INVARIANT(assert_symbol_index, unsigned i, struct gram_parser_spec const *spec) {
    check(i >= 1 && i <= spec->stats.symbols);
}

INVARIANT(assert_rule_index, unsigned i, struct gram_parser_spec const *spec) {
    check(i >= 1 && i <= spec->stats.symbols);
}

#endif

#pragma clang diagnostic pop

#endif // GRAM_ASSERT_C_
