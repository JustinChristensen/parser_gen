#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <base/base.h>
#include <base/bitset.h>
#include <base/rbtree.h>
#include <regex/nfa.h>
#include "gram/spec.h"
#include "gram/analyze.h"
#include "gram/slr.h"

#include "internal/macros.c"

struct states_context {
    gram_state_no nstates;
    struct bitset *ruleset;
    bool *added;
    struct slr_itemset *kernel;
    struct slr_transitions *trans;
};

static struct slr_itemset *
make_itemset(unsigned maxitems, unsigned nitems, struct slr_item *items) {
    struct slr_itemset *itemset = malloc(sizeof *itemset + sizeof (struct slr_item) * maxitems);
    if (!itemset) return NULL;
    itemset->nitems = nitems;
    memcpy(itemset->items, items, sizeof (struct slr_item) * nitems);
    return itemset;
}

static struct slr_transitions *
make_transitions(unsigned maxstates, unsigned nstates, struct slr_state *states) {
    struct slr_transitions *trans = malloc(sizeof *trans + sizeof (struct slr_state *) * maxstates);
    if (!trans) return NULL;
    trans->nstates = nstates;
    if (states) memcpy(trans->states, states, sizeof (struct slr_item) * nstates);
    return itemset;
}

static struct slr_state *
make_state(struct slr_itemset *itemset) {
    if (!itemset) return NULL;

    itemset = make_itemset(itemset->nitems, itemset->nitems, itemset->items);
    if (!itemset) return NULL;

    struct slr_state *state = malloc(sizeof *state);
    if (!state) return free(itemset), NULL;

    *state = (struct slr_state) {
        .itemset = itemset
    };

    return state;
}

static struct slr_state *
promote_kernel(struct gram_parser_spec const *spec, struct states_context *context) {
    struct gram_stats const stats = spec->stats;
    struct slr_itemset *kernel = context->kernel;
    struct slr_item *item = &kernel->items[kernel->nitems];

    struct bsiter it = bsiter(context->ruleset);
    unsigned rule;
    while (bsnext(&rule, &it)) *item++ = (struct slr_item) { rule, 0 };

    struct slr_state *state = make_state(kernel);

    bszero(context->ruleset);

    kernel->nitems = 0;
    // struct slr_item const zero = { 0 };
    // for (unsigned i = 0; i < offs(stats.rules); i++)
    //     kernel->items[i] = zero;

    return state;
}

static void
nonterm_closure(gram_sym_no nt, struct gram_parser_spec const *spec, struct states_context *context) {
    invariant(assert_symbol_index, nt, spec);

    struct gram_symbol sym = spec->symbols[nt];

    if (context->added[sym.num]) return;
    context->added[sym.num] = true;

    gram_sym_no const nonterm0 = offs(spec->stats.terms);
    gram_rule_no *r = sym.derives;
    while (*r) {
        bsins(*r, context->ruleset);
        gram_sym_no *s = spec->rules[*r];
        if (*s >= nonterm0) nonterm_closure(*s, spec, context);
    }
}

static struct slr_state *
closure(struct slr_state *last, struct gram_parser_spec const *spec, struct states_context *context) {
    struct gram_stats const stats = spec->stats;
    struct slr_itemset *kernel = context->kernel;
    gram_sym_no const nonterm0 = offs(stats.terms);

    for (unsigned i = 0; i < kernel->nitems; i++) {
        gram_sym_no *s = rules[kernel->items[i].rule];
        if (*s >= nonterm0) nonterm_closure(*s, spec, context);
    }

    // look up itemset to determine if we need to make a state or can use an existing one

    struct slr_state *state = promote_kernel(kernel, context->ruleset);

    memset(context->added, false, offs(stats.symbols));

    if (last) last->next = state;

    return state;
}

static struct slr_state *
goto_(
    struct slr_state *last, gram_sym_no s, struct slr_state *state,
    struct gram_parser_spec const *spec, struct states_context *context
) {
    struct slr_itemset
        *itemset = state->itemset,
        *kernel = context->kernel;
    struct slr_item *kitem = kernel->items;
    unsigned nkitems = kernel->nitems;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct slr_item item = itemset->items[i];

        if (spec->rules[item.rule][item.pos] == s) {
            *kitem++ = (struct slr_item) { item.rule, item.pos + 1 };
            nkitems++;
        }
    }

    kernel->nitems = nkitems;

    last = closure(last, spec, context);

    struct slr_transitions *trans = context->trans;
    trans->states[trans->nstates++] = last;

    return last;
}

static struct slr_state *
reach_states(
    struct slr_state *last, struct slr_state *state,
    struct gram_parser_spec const *spec, struct states_context *context
) {
    struct slr_transitions *trans = context->trans;
    struct gram_symbol *s = gram_symbol0(spec);
    while (*s) {
        last = goto_(last, *s, state, spec,  context);
        s++;
    }

    state->trans = make_transitions(trans->nitems, trans->nitems, trans->items);

    return last;
}

static struct slr_state *
_discover_states(struct gram_parser_spec const *spec, struct states_context *context) {
    struct gram_stats const stats = spec->stats;

    struct slr_state *last = closure(NULL, spec, context);
    struct slr_state *state0 = last;

    for (struct slr_state *state = state0; state; state = state->next)
        last = reach_states(last, state, spec, context);

    return state0;
}

static bool states_context(struct states_context *context, struct gram_stats const stats) {
    struct bitset *ruleset = bitset(offs(stats.rules));
    if (!ruleset) return false;

    bool *added = calloc(offs(stats.symbols), sizeof *added);
    if (!added) return free(ruleset), false;

    struct slr_transitions *trans = make_transitions(stats.symbols, 0, NULL);
    if (!trans) return freel(ruleset, added), false;

    struct slr_item item0 = { GM_START, 0 };
    struct slr_itemset *kernel = make_itemset(stats.rules, 1, &item0);
    if (!kernel) return freel(ruleset, added, trans), false;

    *context = (struct states_context) {
        .ruleset = ruleset,
        .added = added,
        .trans = trans,
        .kernel = kernel
    }

    return true;
}

static void free_states_context(struct states_context *context) {
    freel(
        context->ruleset,
        context->added,
        context->trans,
        context->kernel
    );
}

static struct slr_state *
discover_states(struct gram_parser_spec const *spec) {
    struct states_context context = { 0 };
    if (!states_context(&context)) return NULL;
    struct slr_state *states = _discover_states(spec, &context);
    free_states_context(&context);
    return states;
}

// struct slr_parser slr_parser(
//     struct nfa_context scanner, struct gram_stats stats
// ) {
// }

bool gen_slr(
    struct slr_error *error, struct slr_parser *parser,
    struct gram_parser_spec *spec
) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);



    return true;
}


// void print_slr_parser(FILE *handle, struct slr_parser *parser);
// void fre_slr_parser(struct slr_parser *parser);
//
// void print_slr_error(FILE *handle, struct slr_error error);
