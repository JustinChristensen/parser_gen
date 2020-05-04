#ifndef GRAM_SLR_H_
#define GRAM_SLR_H_ 1

#include <stdbool.h>
#include "gram/analyze.h"
#include "gram/lr.h"
#include "gram/spec.h"

struct lr_action **slr_table(
    struct lr_error *error, unsigned *nstates,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san, gram_sym_no const *derived_by,
    struct gram_parser_spec const *spec
);

#endif // GRAM_SLR_H_
