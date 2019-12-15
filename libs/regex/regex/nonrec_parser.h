#ifndef REGEX_NONREC_PARSER_H_
#define REGEX_NONREC_PARSER_H_ 1

#include <stdbool.h>
#include "result_types.h"

/**
 * LL(1)
 * Top-Down
 * Predictive
 * Iterative
 */
bool parse_regex_nonrec(struct parse_context *context);

#endif // REGEX_NONREC_PARSER_H_
