#ifndef GRAM_SLR_H_
#define GRAM_SLR_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/spec.h"

typedef unsigned gram_pos_no;
typedef unsigned gram_state_no;

enum slr_error_type {
    GM_SLR_SYNTAX_ERROR,
    GM_SLR_NOT_SLR_ERROR,
    GM_SLR_SCANNER_ERROR,
    GM_SLR_OOM_ERROR
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
    struct nfa_context scanner;
};

struct slr_parser_state {
    struct slr_parser *parser;
    struct nfa_match match;
    gram_sym_no lookahead;
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

struct slr_item {
    gram_rule_no rule;
    gram_pos_no pos;
};

struct slr_itemset {
    unsigned nitems;
    struct slr_item items[];
};

struct slr_transitions {
    unsigned nstates;
    struct slr_state *states[];
};

struct slr_state {
    gram_state_no num;
    gram_sym_no sym;
    struct slr_itemset *itemset;
    struct slr_transitions *trans;
};

struct slr_parser slr_parser(
    struct nfa_context scanner, struct gram_stats stats
);
bool gen_slr(
    struct slr_error *error, struct slr_parser *parser,
    struct gram_parser_spec *spec
);
void print_slr_parser(FILE *handle, struct slr_parser *parser);
void fre_slr_parser(struct slr_parser *parser);

void print_slr_error(FILE *handle, struct slr_error error);

#endif // GRAM_SLR_H_
