#ifndef GRAM_ANALYZE_H_
#define GRAM_ANALYZE_H_ 1

#include <base/intset.h>
#include "gram/spec.h"

bool gram_first(struct intset **set, unsigned int i, struct gram_parser_spec *spec);
struct intset **gram_firsts(struct gram_parser_spec *spec);
void free_gram_firsts(struct intset **sets, struct gram_parser_spec *spec);
void print_gram_firsts(FILE *handle, struct intset **sets, struct gram_parser_spec *spec);
struct intset *gram_follow(unsigned int num, struct gram_parser_spec *spec);

#endif // GRAM_ANALYZE_H_

