#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <stdbool.h>
#include <base/bitset.h>
#include "gram/spec.h"

// enum ll1_check_result {
//     GM_IS_LL1,
//     GM_LL1_FIRST_FIRST_CONFLICT,
//     GM_LL1_FIRST_FOLLOW_CONFLICT,
//     GM_LL1_DIRECTLY_LEFT_RECURSIVE,
//     GM_LL1_INDIRECTLY_LEFT_RECURSIVE,
//     GM_LL1_AMBIGUOUS
// };

struct gram_symbol_analysis {
    unsigned nsymbols;
    bool *nullable;
    struct bitset **firsts;
    struct bitset **follows;
};

struct gram_rule_analysis {
    unsigned nrules;
    bool *nullable;
    struct bitset **ffollows;
};

void gram_count(struct gram_parser_spec *spec);
void print_gram_stats(FILE *handle, struct gram_stats const stats);

bool gram_analyze_symbols(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec);
void free_gram_symbol_analysis(struct gram_symbol_analysis *an);
void print_gram_symbol_analysis(FILE *handle, struct gram_symbol_analysis const *an);

bool gram_analyze_rules(struct gram_rule_analysis *an, struct gram_symbol_analysis *syman, struct gram_parser_spec const *spec);
void free_gram_rule_analysis(struct gram_rule_analysis *an);
void print_gram_rule_analysis(FILE *handle, struct gram_rule_analysis const *an);

// bool gram_is_ll1(
//     bool const *nullable, struct bitset **firsts, struct bitset **follows,
//     struct gram_parser_spec const *spec
// );

#endif // GRAM_ANALYZE_H_

