#ifndef GRAM_LL1_H_
#define GRAM_LL1_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/spec.h"

enum ll1_error_type {
    GM_LL1_SYNTAX_ERROR,
    GM_LL1_SCANNER_ERROR,
    GM_LL1_OOM_ERROR
};

struct ll1_error {
    enum ll1_error_type type;
    union {
        struct { struct regex_loc loc; unsigned actual; unsigned expected; };
        struct { char *file; int col; };
        struct regex_error scanerr;
    };
};

struct ll1_parser {
    unsigned int **rtable;
    unsigned int **ptable;
    struct nfa_context scanner;
    struct gram_stats stats;
};

struct ll1_parser_state {
    struct ll1_parser *parser;
    struct nfa_match match;
    unsigned lookahead;
};

struct ll1_parser ll1_parser(
    struct nfa_context scanner, unsigned int **rtable, unsigned int **ptable,
    struct gram_stats stats
);
bool gen_ll1(
    struct ll1_error *error, struct ll1_parser *parser,
    struct gram_parser_spec *spec
);
void print_ll1_parser(FILE *handle, struct ll1_parser *parser);
void free_ll1_parser(struct ll1_parser *parser);

struct ll1_parser_state ll1_parser_state(struct ll1_parser *parser);
bool ll1_parse(struct ll1_error *error, char *input, struct ll1_parser_state *state);
void free_ll1_parser_state(struct ll1_parser_state *state);

void print_ll1_error(FILE *handle, struct ll1_error error);

#endif // GRAM_LL1_H_
