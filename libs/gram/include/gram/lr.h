#ifndef GRAM_LR_H_
#define GRAM_LR_H_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/spec.h"

enum lr_error_type {
    GM_LR_SYNTAX_ERROR,
    GM_LR_NOT_SLR_ERROR,
    GM_LR_NOT_LALR_ERROR,
    GM_LR_NOT_LR1_ERROR,
    GM_LR_SCANNER_ERROR,
    GM_LR_OOM_ERROR
};

enum lr_action_type {
    GM_LR_ERROR,
    GM_LR_SHIFT,
    GM_LR_REDUCE,
    GM_LR_GOTO,
    GM_LR_ACCEPT
};

struct lr_action {
    enum lr_action_type action;
    unsigned n;
};

struct lr_rule {
    gram_sym_no nt;
    unsigned n;
};

struct lr_error {
    enum lr_error_type type;
    union {
        struct { struct regex_loc loc; char **symtab; gram_sym_no actual; gram_sym_no *expected; };
        struct { char *file; int col; };
        struct regex_error scanerr;
    };
};

struct lr_parser {
    unsigned nstates;
    struct lr_action const **atable;
    struct lr_rule const *rtable;
    char **symtab;
    gram_sym_no **next_sets;
    struct nfa_context scanner;
    struct gram_stats stats;
};

struct lr_parser_state {
    struct lr_parser *parser;
    struct nfa_match match;
    gram_sym_no lookahead;
};

typedef struct lr_action const **action_table(
    struct lr_error *error, unsigned *nstates, struct lr_rule const *rtable,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san,
    struct gram_parser_spec const *spec
);

action_table slr_table;
action_table lalr_table;
action_table lr1_table;

struct lr_parser lr_parser(
    unsigned const nstates, struct lr_action const **atable, struct lr_rule const *rtable,
    char **symtab, gram_sym_no **next_sets,
    struct nfa_context scanner, struct gram_stats const stats
);
bool gen_lr(
    struct lr_error *error, struct lr_parser *parser, action_table *table,
    struct gram_parser_spec *spec
);
void print_lr_parser(FILE *handle, struct lr_parser const *parser);
void free_lr_parser(struct lr_parser *parser);

struct lr_parser_state lr_parser_state(struct lr_parser const *parser);
bool lr_parse(struct lr_error *error, char *input, char *path, struct lr_parser_state *state);
void free_lr_parser_state(struct lr_parser_state *state);

void print_lr_error(FILE *handle, struct lr_error error);

#endif // GRAM_LR_H_
