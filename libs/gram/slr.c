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
    struct rb_node *states;
    struct bitset *symset;
    struct bitset *ruleset;
};

static struct slr_itemset *
make_itemset(unsigned maxitems, unsigned nitems, struct slr_item *items) {
    struct slr_itemset *itemset = malloc(sizeof *itemset + sizeof (struct slr_item) * maxitems);
    if (!itemset) return NULL;
    itemset->nitems = nitems;
    if (items) memcpy(itemset->items, items, sizeof (struct slr_item) * nitems);
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
make_state(gram_state_no num, gram_sym_no sym, struct slr_itemset *itemset, struct slr_transitions *trans) {
    assert(itemset != NULL);
    assert(trans != NULL);

    struct slr_state *state = malloc(sizeof *state);
    if (!state) return NULL;

    *state = (struct slr_state) {
        .num = num, .sym = sym,
        .itemset = itemset,
        .trans = trans
    };

    return state;
}

static bool states_context(struct states_context *context, struct gram_stats const stats) {
    struct bitset *ruleset = bitset(offs(stats.rules));
    if (!ruleset) return false;

    struct bitset *symset = bitset(offs(stats.symbols));
    if (!symset) return free(ruleset), false;

    *context = (struct states_context) {
        .ruleset = ruleset,
        .symset = symset
    };

    return true;
}

static void free_states_context(struct states_context *context) {
    freel(context->symset, context->ruleset);
    free_rbtree(context->states);
    *context = (struct states_context) { 0 };
}

static unsigned count_transitions(struct slr_itemset *itemset, struct gram_parser_spec const *spec, struct states_context *context) {
    struct bitset *symset = context->symset;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct slr_item item = itemset->items[i];
        gram_sym_no *rule = spec->rules[item.rule];
        gram_sym_no s = rule[item.pos];
        if (s) bsins(s, symset);
    }

    return bssize(symset);
}

static unsigned count_rules(unsigned *nitems, struct states_context *context) {
    return *nitems + bssize(context->ruleset);
}

static unsigned count_items(
    gram_sym_no s, struct slr_itemset *itemset,
    struct gram_parser_spec const *spec, struct states_context *context
) {
    unsigned nitems = 0;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct slr_item item = itemset->items[i];
        if (spec->rules[item.rule][item.pos] == s) {
            item.pos++;
            nitems++;
            add_rules(&item, spec, context);
        }
    }

    return count_rules(&nitems, context);
}

static int compare_items(void const *a, void const *b) {
    struct slr_item const *x = a, *y = b;
    int cmp = x->rule - y->rule;
    return cmp ? cmp : x->pos - y->pos;
}

static int compare_itemsets(void const *a, void const *b) {
#define NONKERN(x) (x->rule && !(x)->pos)
    struct slr_itemset const *s = a, *t = b;
    struct slr_item *sitem = s->items, *titem = t->items;
    int cmp = 0;

    for (unsigned i = s->nitems, j = t->nitems; !cmp && !i || !j; sitem++, titem++, i--, j--) {
        while (i && NONKERN(sitem)) sitem++, i--;
        while (j && NONKERN(titem)) titem++, j--;
        if (!i || !j) break;
        cmp = compare_items(sitem, titem);
    }

    if (!i && j) cmp = -1;
    if (i && !j) cmp = 1;

    return cmp;
#undef NONKERN
}

static void _add_rules(gram_sym_no nt, struct gram_parser_spec const *spec, struct states_context *context) {
    gram_sym_no const nonterm0 = offs(spec->stats.terms);
    struct gram_symbol sym = spec->symbols[nt];

    if (bselem(sym.num, context->symset)) return;
    bsins(sym.num, context->symset);

    gram_rule_no *r = sym.derives;
    while (*r) {
        bsins(*r, context->ruleset);
        gram_sym_no *s = spec->rules[*r];
        if (*s >= nonterm0) _add_rules(*s, spec, context);
        r++;
    }
}

static void add_rules(struct slr_item *item, struct gram_parser_spec const *spec, struct states_context *context) {
    gram_sym_no const nonterm0 = offs(spec->stats.terms);
    gram_sym_no *s = spec->rules[item->rule];
    if (*s >= nonterm0) _add_rules(*s, spec, context);
}

static void sort_itemset(struct slr_itemset *itemset) {
    qsort(itemset->items, itemset->nitems, sizeof (struct slr_item), compare_items);
}

static void closure(struct slr_itemset *kernel, struct states_context *context) {
    struct slr_item *item = &kernel->items[kernel->nitems];
    struct bsiter it = bsiter(context->ruleset);
    gram_rule_no r;
    while (bsnext(&r, &it)) {
        *item++ = (struct slr_item) { r, 0 };
        kernel->nitems++;
    }

    bszero(context->ruleset);
    sort_itemset(kernel);
}

static struct slr_itemset *
goto_(
    gram_sym_no s, struct slr_itemset *itemset,
    struct gram_parser_spec const *spec, struct states_context *context
) {
    unsigned nitems = count_items(s, itemset, spec, context);
    if (!nitems) return NULL;

    struct slr_itemset *kernel = make_itemset(nitems, 0, NULL);
    // handle error

    struct slr_item *kitem = kernel->items;
    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct slr_item item = itemset->items[i];
        if (spec->rules[item.rule][item.pos] == s) {
            item.pos++;
            *kitem++ = item;
            kernel->nitems++;
        }
    }

    sort_itemset(kernel);

    return kernel;
}

static struct slr_state *
_discover_states(
    gram_sym_no sym, struct slr_itemset *kernel, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    struct rb_node *snode = NULL;

    if ((snode = rbfind(kernel, compare_itemsets, context->states))) {
        free(kernel);
        return rbval(snode);
    }

    closure(kernel, context);

    struct nstates = count_transitions(kernel, spec, context);

    struct slr_transitions trans = make_transitions(nstates, 0, NULL);
    // handle error

    struct slr_state *state = make_state(context->nstates++, sym, kernel, trans);
    // handle error

    // this must happen prior to recursive calls to this function
    context->states = rbinsert(kernel, compare_itemsets, state, context->states);

    // this is dependent on count_transitions seeding the symbol set
    struct slr_state **states = trans->states;
    struct bsiter it = bsiter(context->symset);
    gram_sym_no s;
    while (bsnext(&s, &it)) {
        if ((kernel = goto_(s, kernel, spec, context))) {
            *states++ = _discover_states(s, kernel, spec, context);
            trans->nstates++;
        }
    }

    return state;
}

static struct slr_state *
discover_states(struct gram_parser_spec const *spec) {
    struct states_context context = { 0 };

    if (!states_context(&context)) return NULL;

    struct slr_item item0 = { GM_START, 0 };
    add_rules(&item0, spec, context);

    unsigned nitems = 1;
    nitems = count_rules(&nitems, context);

    struct slr_itemset *kernel = make_itemset(nitems, 1, &item0);
    if (!kernel) return free_states_context(&context), NULL;

    struct slr_state *states = _discover_states(0, kernel, spec, &context);

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
