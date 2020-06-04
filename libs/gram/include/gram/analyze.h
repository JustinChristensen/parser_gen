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
    GM_NULL_AMBIGUITY,
    GM_LEFT_RECURSION
    // GM_SHIFT_REDUCE,
    // GM_REDUCE_REDUCE
};

struct gram_conflict {
    enum gram_conflict_type type;
    union {
        // first-first, first-follows
        struct {
            gram_sym_no nonterm;
            union {
                gram_rule_no rules[2];
                gram_rule_no rule;
            };
        };
        // left recursion
        struct {
            gram_sym_no *derivations;
            unsigned n;
        };
    };
    struct gram_conflict *next;
};

enum gram_class {
    GM_NONE,
    GM_LR1,
    GM_LALR,
    GM_SLR,
    GM_LL
};

struct gram_analysis {
    enum gram_class clas;
    struct gram_conflict *conflicts;
};

void gram_count(struct gram_parser_spec *spec);
void print_gram_stats(FILE *handle, struct gram_stats const stats);

bool gram_analyze_symbols(struct gram_symbol_analysis *an, struct gram_parser_spec const *spec);
void free_gram_symbol_analysis(struct gram_symbol_analysis *an);
void print_gram_symbol_analysis(FILE *handle, struct gram_symbol_analysis const *an);

bool gram_analyze_rules(struct gram_rule_analysis *an, struct gram_symbol_analysis *san, struct gram_parser_spec const *spec);
void free_gram_rule_analysis(struct gram_rule_analysis *an);
void print_gram_rule_analysis(FILE *handle, struct gram_rule_analysis const *an);

bool gram_analyze(struct gram_analysis *an, struct gram_symbol_analysis *san, struct gram_parser_spec const *spec);
void free_gram_analysis(struct gram_analysis *an);
void print_gram_analysis(FILE *handle, struct gram_analysis *an, struct gram_parser_spec const *spec);

#endif // GRAM_ANALYZE_H_

