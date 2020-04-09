#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <stdbool.h>
#include <base/intset.h>
#include "gram/spec.h"

struct intset **gram_firsts(bool const *nullable, struct gram_parser_spec const *spec);
void free_gram_sets(struct intset **sets, struct gram_parser_spec const *spec);
void print_gram_sets(FILE *handle, struct intset **sets, struct gram_parser_spec const *spec);

void print_gram_stats(FILE *handle, struct gram_stats const stats);
bool *gram_nullable(struct gram_parser_spec const *spec);
void print_gram_nullable(FILE *handle, bool const *nullable, struct gram_parser_spec const *spec);

#endif // GRAM_ANALYZE_H_

