#ifndef GRAM_LL1_H_
#define GRAM_LL1_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/spec.h"

struct ll1_parser {
    unsigned int **rtable;
    unsigned int **ptable;
    struct nfa_context scanner;
    struct gram_stats stats;
};

struct ll1_parser ll1_parser(
    struct nfa_context scanner, unsigned int **rtable, unsigned int **ptable,
    struct gram_stats stats
);
bool gen_ll1(
    struct ll1_error *error, struct ll1_parser *parser,
    struct gram_parser_spec *spec
);
bool ll1_parse(char *input, struct ll1_parser const *parser);
void print_ll1_parser(FILE *handle, struct ll1_parser *parser);
void free_ll1_parser(struct ll1_parser *parser);


#endif // GRAM_LL1_H_
