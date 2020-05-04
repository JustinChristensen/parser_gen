#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/array.h>
#include <base/base.h>
#include <base/assert.h>
#include <regex/nfa.h>
#include "gram/spec.h"
#include "gram/analyze.h"
#include "gram/states.h"
#include "gram/lr.h"
#include "gram/slr.h"

#include "internal/assert.c"
#include "internal/lr.c"
#include "internal/macros.c"
#include "internal/spec.c"

static bool not_slr_error(struct lr_error *error) {
    prod(error, ((struct lr_error) { .type = GM_LR_NOT_SLR_ERROR }));
    return false;
}

struct lr_action **slr_table(
    struct lr_error *error, unsigned *nstates,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san, gram_sym_no const *derived_by,
    struct gram_parser_spec const *spec
) {
    if (gan->clas < GM_SLR) return not_slr_error(error), NULL;

    unsigned _nstates = 0;
    struct lr_state *states = NULL;
    if ((states = discover_lr_states(&_nstates, GM_LR0_ITEMS, san, spec)) == NULL)
        return oom_error(error, NULL), NULL;

    struct gram_stats const stats = spec->stats;
    unsigned nsymbols = offs(stats.symbols);
    size_t atsize = _nstates * sizeof (struct lr_action *) + _nstates * nsymbols * sizeof (struct lr_action);
    struct lr_action **atable = malloc(atsize);
    if (!atable) return oom_error(error, NULL), free_lr_states(_nstates, states), NULL;

    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return oom_error(error, atable), free_lr_states(_nstates, states), NULL;

    memset(atable, 0, atsize);
    struct lr_action *rows = (struct lr_action *) (atable + _nstates);

    gram_sym_no const nonterm0 = offs(stats.terms);
    struct lr_state *state = states;

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (atable[state->num]) continue;

        struct lr_action *row = rows + state->num * nsymbols;
        atable[state->num] = row;

        // reductions
        struct lr_itemset *itemset = state->itemset;
        for (unsigned i = 0; i < itemset->nitems; i++) {
            struct lr_item item = itemset->items[i];
            gram_sym_no *rule = spec->rules[item.rule];

            if (item.rule != GM_START && !rule[item.pos]) {
                gram_sym_no nt = derived_by[item.rule];
                struct bsiter it = bsiter(san->follows[nt]);
                gram_sym_no s;
                while (bsnext(&s, &it)) {
                    struct lr_action act = REDUCE(item.pos, nt);
                    invariant(action_table_conflict, row, act, s, state->num);
                    row[s] = act;
                }
            }
        }

        // shifts, gotos, accept
        struct lr_transitions *trans = state->trans;
        for (unsigned i = 0; i < trans->nstates; i++) {
            struct lr_state *next = trans->states[i];

            struct lr_action act;
            if (next->sym == GM_EOF)       act = ACCEPT();
            else if (next->sym < nonterm0) act = SHIFT(next->num);
            else                           act = GOTO(next->num);

            invariant(action_table_conflict, row, act, next->sym, state->num);
            row[next->sym] = act;

            apush(&next, stack);
        }
    }

    free_lr_states(_nstates, states);
    free_array(stack);

    *nstates = _nstates;

    return atable;
}

