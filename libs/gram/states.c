#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/assert.h>
#include <base/array.h>
#include <base/base.h>
#include <base/bitset.h>
#include <base/debug.h>
#include <base/rbtree.h>
#include "gram/spec.h"
#include "gram/states.h"

#include "internal/assert.c"
#include "internal/macros.c"

#define debug(...) debug_ns("gram_states", __VA_ARGS__);

struct states_context {
    gram_state_no nstates;
    struct rb_node *states;
    struct bitset *symset;
    struct bitset *ruleset;
};

static void print_item(FILE *handle, struct lr_item item) {
    fprintf(handle, "(%u, %u)", item.rule, item.pos);
}

static void print_itemset_compact(FILE *handle, void const *a) {
    if (!a) return;
    struct lr_itemset const *itemset = a;
    fprintf(handle, "{");
    if (itemset->nitems) {
        print_item(handle, itemset->items[0]);
        for (unsigned i = 1; i < itemset->nitems; i++) {
            fprintf(handle, ",");
            print_item(handle, itemset->items[i]);
        }
    }
    fprintf(handle, "}");
}

static void print_itemset(FILE *handle, struct lr_itemset *itemset) {
    fprintf(handle, "  itemset:\n");
    for (int i = 0; i < itemset->nitems; i++) {
        fprintf(handle, "    ");
        print_item(handle, itemset->items[i]);
        fprintf(handle, "\n");
    }
}

static void print_transitions(FILE *handle, struct lr_transitions *trans) {
    if (trans->nstates) {
        fprintf(handle, "  transitions:\n");
        for (int i = 0; i < trans->nstates; i++) {
            fprintf(handle, "    %u\n", trans->states[i]->num);
        }
    }
}

static void print_state(FILE *handle, struct lr_state *state) {
    fprintf(handle, "  num: %u\n  sym: %u\n", state->num, state->sym);
    print_itemset(handle, state->itemset);
    print_transitions(handle, state->trans);
}

static void debug_itemset(struct lr_itemset const *itemset) {
    if (debug_is("gram_states")) print_itemset_compact(stderr, itemset);
}

static struct lr_itemset *
make_itemset(unsigned maxitems, unsigned nitems, struct lr_item *items) {
    struct lr_itemset *itemset = malloc(sizeof *itemset + sizeof (struct lr_item) * maxitems);
    if (!itemset) return NULL;
    itemset->nitems = nitems;
    if (items) memcpy(itemset->items, items, sizeof (struct lr_item) * nitems);
    return itemset;
}

static struct lr_transitions *
make_transitions(unsigned maxstates, unsigned nstates, struct lr_state *states) {
    struct lr_transitions *trans = malloc(sizeof *trans + sizeof (struct lr_state *) * maxstates);
    if (!trans) return NULL;
    trans->nstates = nstates;
    if (states) memcpy(trans->states, states, sizeof (struct lr_item) * nstates);
    return trans;
}

static struct lr_state *
make_state(gram_state_no num, gram_sym_no sym, struct lr_itemset *itemset, struct lr_transitions *trans) {
    assert(itemset != NULL);
    assert(trans != NULL);

    struct lr_state *state = malloc(sizeof *state);
    if (!state) return NULL;

    *state = (struct lr_state) {
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

static void add_rules(struct lr_item *item, struct gram_parser_spec const *spec, struct states_context *context) {
    gram_sym_no const nonterm0 = offs(spec->stats.terms);
    gram_sym_no s = spec->rules[item->rule][item->pos];
    if (s >= nonterm0) _add_rules(s, spec, context);
}

static unsigned count_transitions(struct lr_itemset *itemset, struct gram_parser_spec const *spec, struct states_context *context) {
    struct bitset *symset = context->symset;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
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
    gram_sym_no s, struct lr_itemset *itemset,
    struct gram_parser_spec const *spec, struct states_context *context
) {
    unsigned nitems = 0;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
        if (spec->rules[item.rule][item.pos] == s) {
            item.pos++;
            nitems++;
            add_rules(&item, spec, context);
        }
    }

    bszero(context->symset);

    return count_rules(&nitems, context);
}

#define KERN(x) ((x)->rule == GM_START || (x)->pos)
#define NONKERN(x) ((x)->rule > GM_START && !(x)->pos)
static int compare_items(void const *a, void const *b) {
    struct lr_item const *x = a, *y = b;
    int cmp;

    if (KERN(x) && NONKERN(y)) cmp = 1;
    else if (NONKERN(x) && KERN(y)) cmp = -1;
    else {
        cmp = y->rule - x->rule;
        cmp = cmp ? cmp : y->pos - x->pos;
    }

    return -cmp;
}

static int compare_itemsets(void const *a, void const *b) {
    struct lr_itemset const *s = a, *t = b;
    struct lr_item const *sitem = s->items, *titem = t->items;
    int cmp = 0;

    unsigned i, j;
    for (i = s->nitems, j = t->nitems; !cmp && i && j; sitem++, titem++, i--, j--) {
        while (i && NONKERN(sitem)) sitem++, i--;
        while (j && NONKERN(titem)) titem++, j--;
        if (i && j) cmp = compare_items(sitem, titem);
    }
    while (i && NONKERN(sitem)) sitem++, i--;
    while (j && NONKERN(titem)) titem++, j--;

    if (!i && j) cmp = -1;
    if (i && !j) cmp = 1;

    return cmp;
}
#undef KERN
#undef NONKERN

static void sort_itemset(struct lr_itemset *itemset) {
    qsort(itemset->items, itemset->nitems, sizeof (struct lr_item), compare_items);
}

static void closure(struct lr_itemset *kernel, struct states_context *context) {
    struct lr_item *item = &kernel->items[kernel->nitems];

    debug("kernel: "); debug_itemset(kernel); debug("\n");

    struct lr_item nitem = { 0 };
    gram_rule_no r;
    struct bsiter it = bsiter(context->ruleset);
    while (bsnext(&r, &it)) {
        nitem.rule = r;
        *item++ = nitem;
        kernel->nitems++;
    }

    debug("closure: "); debug_itemset(kernel); debug("\n");

    bszero(context->ruleset);
    sort_itemset(kernel);
}

static struct lr_itemset *
goto_(
    gram_sym_no s, struct lr_itemset *itemset,
    struct gram_parser_spec const *spec, struct states_context *context
) {
    unsigned nitems = count_items(s, itemset, spec, context);
    if (!nitems) return NULL;

    struct lr_itemset *kernel = make_itemset(nitems, 0, NULL);
    // handle error

    struct lr_item *kitem = kernel->items;
    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
        if (spec->rules[item.rule][item.pos] == s) {
            item.pos++;
            *kitem++ = item;
            kernel->nitems++;
        }
    }

    sort_itemset(kernel);

    return kernel;
}

static struct lr_state *
_discover_states(
    gram_sym_no sym, struct lr_itemset *kernel, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    struct rb_node *snode = NULL;

    if ((snode = rbfind(kernel, compare_itemsets, context->states))) {
        free(kernel);
        bszero(context->ruleset);
        return rbval(snode);
    }

    closure(kernel, context);
    struct lr_itemset *itemset = kernel;

    unsigned nstates = count_transitions(itemset, spec, context);

    struct lr_transitions *trans = make_transitions(nstates, 0, NULL);
    // handle error

    struct lr_state *state = make_state(context->nstates++, sym, itemset, trans);
    // handle error

    // this must happen prior to recursive calls to this function
    context->states = rbinsert(itemset, compare_itemsets, state, context->states);

    struct lr_state **states = trans->states;
    for (gram_sym_no s = GM_SYMBOL0; s < spec->stats.symbols; s++) {
        if ((kernel = goto_(s, itemset, spec, context))) {
            *states++ = _discover_states(s, kernel, spec, context);
            trans->nstates++;
        }
    }

    return state;
}

struct lr_state *
discover_lr_states(unsigned *nstates, struct gram_parser_spec const *spec) {
    struct states_context context = { 0 };

    if (!states_context(&context, spec->stats)) return NULL;

    struct lr_item item0 = { GM_START, 0 };
    add_rules(&item0, spec, &context);
    bszero(context.symset);

    unsigned nitems = 1;
    nitems = count_rules(&nitems, &context);

    struct lr_itemset *kernel = make_itemset(nitems, 1, &item0);
    if (!kernel) return free_states_context(&context), NULL;

    struct lr_state *states = _discover_states(0, kernel, spec, &context);
    *nstates = context.nstates;

    free_states_context(&context);

    return states;
}

void free_lr_states(unsigned nstates, struct lr_state *state) {
    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return;
    struct lr_state **states = calloc(nstates, sizeof *states);
    if (!states) return free(stack);

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (states[state->num]) continue;
        states[state->num] = state;

        struct lr_transitions *trans = state->trans;
        for (unsigned i = trans->nstates; i > 0; i--) {
            apush(&trans->states[i - 1], stack);
        }
    }

    for (unsigned i = 0; i < nstates; i++) {
        free(states[i]->itemset);
        free(states[i]->trans);
        free(states[i]);
    }

    free_array(stack);
    free(states);
}

void print_lr_states(FILE *handle, unsigned nstates, struct lr_state *state) {
    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return;
    bool *visited = calloc(nstates, sizeof *visited);
    if (!visited) return free(stack);

    fprintf(handle, "states:\n");

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (visited[state->num]) continue;
        visited[state->num] = true;

        fprintf(handle, "\n");
        print_state(handle, state);

        struct lr_transitions *trans = state->trans;
        for (unsigned i = trans->nstates; i > 0; i--) {
            apush(&trans->states[i - 1], stack);
        }
    }

    free_array(stack);
    free(visited);
}

