#ifndef GRAM_ECHO_H_
#define GRAM_ECHO_H_ 1

#include <stdio.h>
#include "gram/ast.h"

void echo_gram_pspec_stats(FILE *handle, struct gram_parser_spec *spec);
void echo_gram_pspec(FILE *handle, struct gram_parser_spec *spec);
void echo_gram_pattern_def(FILE *handle, struct gram_pattern_def *def);
void echo_gram_rule(FILE *handle, struct gram_rule *rule);
void echo_gram_alt(FILE *handle, struct gram_alt *alt);
void echo_gram_rhs(FILE *handle, struct gram_rhs *rhs);

#endif // GRAM_ECHO_H_ 1


