#ifndef REGEX_PARSER_REC_H_
#define REGEX_PARSER_REC_H_ 1

#include <stdbool.h>
#include "parser.h"

/**
 * LL(1)
 * Top-Down
 * Predictive
 * Recursive
 */

bool parse_regex(char *regex, struct parse_context *context);
bool parse_alts(struct parse_context *context);
bool parse_alt(struct parse_context *context);
bool parse_ranges(struct parse_context *context);
bool parse_factor(struct parse_context *context);
bool parse_unops(struct parse_context *context);

#endif // REGEX_PARSER_REC_H_
