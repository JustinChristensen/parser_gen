#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <stdbool.h>
#include <base/intset.h>
#include "gram/spec.h"

struct gram_sets {
    unsigned int n;
    struct intset *sets[];
};

// bool gram_first_follows(struct gram_sets *firsts, struct gram_sets *follows, struct gram_parser_spec *spec);
// void free_gram_sets(struct gram_sets *sets);
// void print_gram_sets(FILE *handle, struct gram_sets const *sets);

void print_gram_stats(FILE *handle, struct gram_stats const stats);
bool *gram_nullable(struct gram_parser_spec *spec);
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_stats stats);

#endif // GRAM_ANALYZE_H_

