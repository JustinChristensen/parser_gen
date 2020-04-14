#ifndef GRAM_LL1_H_
#define GRAM_LL1_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/spec.h"

struct ll1_parser {
    struct nfa_context scanner;
    unsigned int **parse;
    unsigned int **rules;
};

struct ll1_parser ll1_parser(struct nfa_context scanner, unsigned int **parse, unsigned int **rules);
bool genll1(struct ll1_error *error, struct ll1_parser *parser, struct gram_parser_spec *spec);
bool ll1_parse(char *input, struct ll1_parser const *parser);
void free_ll1_parser(struct ll1_parser *parser);


#endif // GRAM_LL1_H_
