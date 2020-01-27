#ifndef REGEX_PARSER_NONREC_H_
#define REGEX_PARSER_NONREC_H_ 1

#include <stdbool.h>
#include <base/array.h>
#include "result_types.h"
#include "regex/parser.h"

/**
 * LL(1)
 * Top-Down
 * Predictive
 * Iterative
 */

#define PARSE_STACK_SIZE 7

bool parse_regex_nonrec(char *regex, struct parse_context *context);
void print_parse_table(enum gram_production **table);

#endif // REGEX_PARSER_NONREC_H_
