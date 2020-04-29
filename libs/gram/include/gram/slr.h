#ifndef GRAM_SLR_H_
#define GRAM_SLR_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/spec.h"

enum slr_error_type {
    GM_SLR_SYNTAX_ERROR,
    GM_SLR_NOT_SLR_ERROR,
    GM_SLR_SCANNER_ERROR,
    GM_SLR_OOM_ERROR
};

enum slr_action_type {
    GM_SLR_ERROR,
    GM_SLR_SHIFT,
    GM_SLR_REDUCE,
    GM_SLR_GOTO,
    GM_SLR_ACCEPT
};

struct slr_action {
    enum slr_action_type action;
    unsigned n;
};

struct slr_error {
    enum slr_error_type type;
    union {
        struct { struct regex_loc loc; gram_sym_no actual; gram_sym_no expected; };
        struct { char *file; int col; };
        struct regex_error scanerr;
    };
};

struct slr_parser {
    unsigned nstates;
    struct slr_action **atable;
    struct nfa_context scanner;
    struct gram_stats stats;
};

struct slr_parser_state {
    struct slr_parser *parser;
    struct nfa_match match;
    gram_sym_no lookahead;
};

struct slr_parser slr_parser(
    unsigned nstates, struct slr_action **atable, struct nfa_context scanner,
    struct gram_stats const stats
);
bool gen_slr(
    struct slr_error *error, struct slr_parser *parser,
    struct gram_parser_spec *spec
);
void print_slr_parser(FILE *handle, struct slr_parser *parser);
void free_slr_parser(struct slr_parser *parser);

void print_slr_error(FILE *handle, struct slr_error error);

#endif // GRAM_SLR_H_
