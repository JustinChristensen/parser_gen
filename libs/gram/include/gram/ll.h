#ifndef GRAM_LL_H_
#define GRAM_LL_H_ 1

#include <stdbool.h>
#include <base/bitset.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/spec.h"

enum ll_error_type {
    GM_LL_SYNTAX_ERROR,
    GM_LL_NOT_LL1_ERROR,
    GM_LL_SCANNER_ERROR,
    GM_LL_OOM_ERROR
};

struct ll_error {
    enum ll_error_type type;
    union {
        struct { struct regex_loc loc; char **symtab; gram_sym_no actual; struct bitset *expected; };
        struct { char *file; int col; };
        struct regex_error scanerr;
    };
};

struct ll_parser {
    gram_sym_no **rtable;
    gram_rule_no **ptable;
    char **symtab;
    struct gram_symbol_analysis san;
    struct nfa_context scanner;
    struct gram_stats stats;
};

struct ll_parser_state {
    struct ll_parser *parser;
    struct nfa_match match;
    gram_sym_no lookahead;
};

struct ll_parser ll_parser(
    struct nfa_context scanner, gram_sym_no **rtable, gram_rule_no **ptable,
    char **symtab, struct gram_symbol_analysis san, struct gram_stats stats
);
bool gen_ll(
    struct ll_error *error, struct ll_parser *parser,
    struct gram_parser_spec *spec
);
void print_ll_parser(FILE *handle, struct ll_parser const *parser);
void free_ll_parser(struct ll_parser *parser);

struct ll_parser_state ll_parser_state(struct ll_parser const *parser);
bool ll_parse(struct ll_error *error, char *input, struct ll_parser_state *state);
void free_ll_parser_state(struct ll_parser_state *state);

void print_ll_error(FILE *handle, struct ll_error error);

#endif // GRAM_LL_H_
