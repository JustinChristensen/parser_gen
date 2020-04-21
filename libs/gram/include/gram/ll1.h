#ifndef GRAM_LL1_H_
#define GRAM_LL1_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/spec.h"

enum ll1_error_type {
    GM_LL1_SYNTAX_ERROR,
    GM_LL1_NOT_LL1_ERROR,
    GM_LL1_SCANNER_ERROR,
    GM_LL1_OOM_ERROR
};

struct ll1_error {
    enum ll1_error_type type;
    union {
        struct { struct regex_loc loc; gram_sym_no actual; gram_sym_no expected; };
        struct { char *file; int col; };
        struct regex_error scanerr;
    };
};

struct ll1_parser {
    gram_sym_no **rtable;
    gram_rule_no **ptable;
    struct nfa_context scanner;
    struct gram_stats stats;
};

struct ll1_parser_state {
    struct ll1_parser *parser;
    struct nfa_match match;
    gram_sym_no lookahead;
};

struct ll1_parser ll1_parser(
    struct nfa_context scanner, gram_sym_no **rtable, gram_rule_no **ptable,
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
