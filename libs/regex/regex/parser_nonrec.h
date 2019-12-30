#ifndef REGEX_PARSER_NONREC_H_
#define REGEX_PARSER_NONREC_H_ 1

#include <stdbool.h>
#include "result_types.h"
#include "regex/parser_shared.h"
#include <base/array.h>

/**
 * LL(1)
 * Top-Down
 * Predictive
 * Iterative
 */

#define PARSE_STACK_SIZE 7

void push_sym(int sym, struct array *stack);
void push_production_symbols(enum gram_production production, struct array *stack);
bool is_terminal(int sym);
bool parse_regex_nonrec(struct parse_context *context);
void print_parse_table(enum gram_production **table);

#endif // REGEX_PARSER_NONREC_H_
