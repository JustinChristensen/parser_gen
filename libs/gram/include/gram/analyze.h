#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <stdbool.h>
#include <base/intset.h>
#include <regex/nfa.h>
#include "gram/spec.h"

enum ll1_error_type {
    GM_LL1_SCANNER_ERROR
};

struct ll1_error {
    enum ll1_error_type type;
    union {
        struct regex_error scanerr;
        struct { char *file; int col; };
    };
};


struct gram_stats gram_count(struct gram_parser_spec const *spec);
void print_gram_stats(FILE *handle, struct gram_stats const stats);

bool *gram_nullable(struct gram_parser_spec const *spec);
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_parser_spec const *spec);

struct intset **gram_firsts(bool const *nullable, struct gram_parser_spec const *spec);
struct intset **gram_follows(bool const *nullable, struct intset **firsts, struct gram_parser_spec const *spec);

void free_gram_sets(struct intset **sets, struct gram_parser_spec const *spec);
void print_gram_sets(FILE *handle, struct intset **sets, struct gram_parser_spec const *spec);

bool gram_is_ll1(
    struct ll1_error *error,
    bool const *nullable, struct intset **firsts, struct intset **follows,
    struct gram_parser_spec const *spec
);

#endif // GRAM_ANALYZE_H_

