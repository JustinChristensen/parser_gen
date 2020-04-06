#ifndef GRAM_LL_H_
#define GRAM_LL_H_ 1

#include <stdbool.h>
#include "gram/spec.h"

struct gram_ll_parser {
    unsigned int **parse_table;
    unsigned int *rule_table;
};

enum gram_ll_error_type {
};

struct gram_ll_error {
};

bool genll(struct gram_ll_error *error, struct gram_ll_parser *parser, struct gram_parser_spec *spec);

#endif // GRAM_LL_H_
