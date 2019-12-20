#ifndef REGEX_PARSER_NONREC_H_
#define REGEX_PARSER_NONREC_H_ 1

#include <stdbool.h>
#include "result_types.h"
#include "regex/parser_shared.h"

/**
 * LL(1)
 * Top-Down
 * Predictive
 * Iterative
 */

#define PARSE_STACK_SIZE 7

void push_sym(int sym, struct array *stack);
bool parse_regex_nonrec(struct parse_context *context);

#endif // REGEX_PARSER_NONREC_H_
