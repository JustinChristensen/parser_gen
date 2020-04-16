#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <stdbool.h>
#include <base/bitset.h>
#include <regex/nfa.h>
#include "gram/spec.h"

enum ll1_error_type {
    GM_LL1_SCANNER_ERROR,
    GM_LL1_OOM_ERROR
};

struct ll1_error {
    enum ll1_error_type type;
    union {
        struct regex_error scanerr;
        struct { char *file; int col; };
    };
};

void gram_count(struct gram_parser_spec *spec);
void print_gram_stats(FILE *handle, struct gram_stats const stats);

bool *gram_nullable(struct gram_parser_spec const *spec);
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_parser_spec const *spec);

void free_gram_sets(struct bitset **sets, struct gram_stats const stats);
void print_gram_sets(FILE *handle, struct bitset **sets, struct gram_stats const stats);

struct bitset **gram_firsts(bool const *nullable, struct gram_parser_spec const *spec);
struct bitset **gram_follows(bool const *nullable, struct bitset **firsts, struct gram_parser_spec const *spec);

bool gram_is_ll1(
    struct ll1_error *error,
    bool const *nullable, struct bitset **firsts, struct bitset **follows,
    struct gram_parser_spec const *spec
);

void print_ll1_error(FILE *handle, struct ll1_error error);

#endif // GRAM_ANALYZE_H_

