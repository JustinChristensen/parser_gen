#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <stdbool.h>
#include <base/bitset.h>
#include "gram/spec.h"

struct gram_symbol_analysis {
    unsigned nsymbols;
    bool *nullable;
    struct bitset **firsts;
    struct bitset **follows;
};

struct gram_rule_analysis {
    unsigned nrules;
    bool *nullable;
    struct bitset **firsts;
};

enum gram_conflict_type {
    GM_FIRST_FIRST,
    GM_FIRST_FOLLOWS,
    GM_LEFT_RECURSION,
    GM_NULL_AMBIGUITY,
    GM_AMBIGUITY
};

struct gram_conflict {
    enum gram_conflict_type type;
    union {
        struct { unsigned nonterm; unsigned *rules; };
    };
    struct gram_conflict *next;
};

enum gram_type {
    GM_NONE,
    GM_LL1
};

struct gram_analysis {
    enum gram_type type;
    struct gram_conflict *conflicts;
};

void gram_count(struct gram_parser_spec *spec);
void print_gram_stats(FILE *handle, struct gram_stats const stats);

bool gram_analyze_symbols(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec);
void free_gram_symbol_analysis(struct gram_symbol_analysis *an);
void print_gram_symbol_analysis(FILE *handle, struct gram_symbol_analysis const *an);

bool gram_analyze_rules(struct gram_rule_analysis *an, struct gram_symbol_analysis *syman, struct gram_parser_spec const *spec);
void free_gram_rule_analysis(struct gram_rule_analysis *an);
void print_gram_rule_analysis(FILE *handle, struct gram_rule_analysis const *an);

// bool gram_analyze(struct gram_analysis *an, struct gram_symbol_analysis *syman, struct gram_parser_spec const *spec);
// void free_gram_analysis(struct gram_analysis *an);
// void print_gram_analysis(struct gram_analysis *an);

#endif // GRAM_ANALYZE_H_

