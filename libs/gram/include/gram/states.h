#ifndef GRAM_STATES_H_
#define GRAM_STATES_H_ 1

#include <stdbool.h>
#include "gram/analyze.h"
#include "gram/spec.h"

typedef unsigned gram_pos_no;
typedef unsigned gram_state_no;

struct lr_item {
    gram_rule_no rule;
    gram_pos_no pos;
    gram_sym_no sym; // the 1 in LR(1)
};

enum lr_item_type {
    GM_LR0_ITEMS,
    GM_LR1_ITEMS
};

struct lr_itemset {
    unsigned nitems;
    struct lr_item items[];
};

struct lr_transitions {
    unsigned nstates;
    struct lr_state *states[];
};

struct lr_state {
    gram_state_no num;
    gram_sym_no sym;
    struct lr_itemset *itemset;
    struct lr_transitions *trans;
};

struct lr_state *discover_lr_states(
    unsigned *nstates, enum lr_item_type item_type,
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec
);
void free_lr_states(unsigned nstates, struct lr_state *state);
void print_lr_states(FILE *handle, unsigned nstates, struct lr_state *state);
int print_lr_states_dot(FILE *handle, unsigned nstates, struct lr_state *state);

#endif // GRAM_STATES_H_
