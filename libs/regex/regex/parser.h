#ifndef REGEX_PARSER_H_
#define REGEX_PARSER_H_ 1

#include <stdbool.h>
#include "parser_shared.h"
#include "result_types.h"

/**
 * LL(1)
 * Top-Down
 * Predictive
 * Recursive
 */

bool parse_regex(struct parse_context *context);
bool parse_expr(struct parse_context *context);
bool parse_alt(struct parse_context *context);
bool parse_cat(struct parse_context *context);
bool parse_factor(struct parse_context *context);

#endif // REGEX_PARSER_H_
