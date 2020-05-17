#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/assert.h>
#include <base/array.h>
#include <base/base.h>
#include <base/bitset.h>
#include <base/macros.h>
#include <base/debug.h>
#include <base/graphviz.h>
#include <base/rbtree.h>
#include <cgraph.h>
#include "gram/spec.h"
#include "gram/states.h"

#include "internal/assert.c"
#include "internal/macros.c"
#include "internal/spec.c"

#define mdebug(...) debug_ns("gram_states_make", __VA_ARGS__);

struct states_stats {
    unsigned itemsets;
    unsigned diffs;
    unsigned merges;
    unsigned finds;
    unsigned inserts;
};

static unsigned ncompares = 0;

struct states_context {
    gram_state_no nstates;
    enum lr_item_type item_type;
    struct rb_node *states;
    struct bitset *symset;
    struct states_stats stats;
};

static void print_stats(FILE *handle, struct states_context *context) {
    struct states_stats stats = context->stats;

    fprintf(handle, "discover_states stats:\n");
    fprintf(handle, "states: %u\nitemsets: %u\ndiffs: %u\nmerges: %u\nfinds: %u\ninserts: %u\ncompares: %u\n",
        context->nstates,
        stats.itemsets,
        stats.diffs,
        stats.merges,
        stats.finds,
        stats.inserts,
        ncompares
    );
}

static void print_item(FILE *handle, struct lr_item item) {
    char *fmt = "(%u, %u)";
    if (item.sym) fmt = "(%u, %u, %u)";
    fprintf(handle, fmt, item.rule, item.pos, item.sym);
}

void print_lr_itemset_compact(FILE *handle, struct lr_itemset const *itemset) {
    if (!itemset) return;
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

static void _print_itemset_compact(FILE *handle, void const *a, void const *b) {
    UNUSED(b);
    return print_lr_itemset_compact(handle, a);
}

static void print_itemset(FILE *handle, struct lr_itemset *itemset, struct gram_parser_spec const *spec) {
    fprintf(handle, "  itemset:\n");
    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
        fprintf(handle, "    ");
        print_item(handle, item);
        // FIXME: reduction: item x rules -> bool
        if (spec && !spec->rules[item.rule][item.pos]) fprintf(handle, " r");
        fprintf(handle, "\n");
    }
}

static void print_transitions(FILE *handle, struct lr_transitions *trans) {
    if (trans->nstates) {
        fprintf(handle, "  transitions:\n");
        for (unsigned i = 0; i < trans->nstates; i++) {
            fprintf(handle, "    %u -> %u\n", trans->states[i]->sym, trans->states[i]->num);
        }
    }
}

void print_lr_state(FILE *handle, struct lr_state const *state, struct gram_parser_spec const *spec) {
    fprintf(handle, "  num: %u\n  sym: %u\n", state->num, state->sym);
    print_itemset(handle, state->itemset, spec);
    print_transitions(handle, state->trans);
}

static void debug_itemset(struct lr_itemset const *itemset) {
    if (debug_is("gram_states_make")) print_lr_itemset_compact(stderr, itemset);
}

static struct lr_itemset *
make_itemset(unsigned maxitems, unsigned kitems, struct lr_item *items) {
    mdebug("making itemset\n");
    struct lr_itemset *itemset = malloc(sizeof *itemset + sizeof *items * maxitems);
    if (!itemset) abort();
    itemset->nitems = itemset->kitems = kitems;
    if (items) memcpy(itemset->items, items, sizeof *items * kitems);
    return itemset;
}

static struct lr_transitions *
make_transitions(unsigned maxstates, unsigned nstates, struct lr_state **states) {
    mdebug("making transitions\n");
    struct lr_transitions *trans = malloc(sizeof *trans + sizeof *states * maxstates);
    if (!trans) abort();
    trans->nstates = nstates;
    if (states) memcpy(trans->states, states, sizeof *states * nstates);
    return trans;
}

static struct lr_state *
make_state(gram_state_no num, gram_sym_no sym, struct lr_itemset *itemset, struct lr_transitions *trans) {
    mdebug("making state\n");
    assert(itemset != NULL);
    assert(trans != NULL);

    struct lr_state *state = malloc(sizeof *state);
    if (!state) abort();

    *state = (struct lr_state) {
        .num = num, .sym = sym,
        .itemset = itemset,
        .trans = trans
    };

    return state;
}

static bool states_context(struct states_context *context, enum lr_item_type item_type, struct gram_stats const stats) {
    size_t setsize = stats.symbols;
    if (item_type != GM_LR0_ITEMS) setsize *= stats.nonterms;

    struct bitset *symset = bitset(setsize);
    if (!symset) return false;

    *context = (struct states_context) {
        .item_type = item_type,
        .symset = symset
    };

    return true;
}

static void free_states_context(struct states_context *context) {
    free(context->symset);
    free_rbtree(context->states);
    *context = (struct states_context) { 0 };
}

static unsigned count_transitions(struct lr_itemset *itemset, struct gram_parser_spec const *spec, struct states_context *context) {
    struct bitset *symset = context->symset;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
        gram_sym_no s = spec->rules[item.rule][item.pos];
        if (s) bsins(s, symset);
    }

    unsigned size = bssize(symset);
    bszero(symset);
    return size;
}

#define KERN(x) ((x)->rule == GM_START || (x)->pos)
#define NONKERN(x) ((x)->rule > GM_START && !(x)->pos)
static int compare_lr0_items(void const *a, void const *b) {
    struct lr_item const *x = a, *y = b;

    int cmp;
    if (KERN(x) && NONKERN(y)) cmp = -1;
    else if (NONKERN(x) && KERN(y)) cmp = 1;
    else {
        cmp = x->rule - y->rule;
        cmp = cmp ? cmp : x->pos - y->pos;
    }

    return cmp;
}

static int compare_lr1_items(void const *a, void const *b) {
    int cmp = compare_lr0_items(a, b);
    if (cmp) return cmp;
    else {
        struct lr_item const *x = a, *y = b;
        return x->sym - y->sym;
    }
}

static int compare_itemsets(
    int (*cmp_items)(void const *a, void const *b), bool const lalr,
    struct lr_itemset const *kernel, struct lr_itemset const *itemset
) {
    invariant(assert_itemsets_sorted, kernel, itemset);

    int cmp = 0;

    struct lr_item const *kitems = kernel->items, *citems = itemset->items;
    unsigned const ni = kernel->kitems, nj = itemset->kitems;
    unsigned i = 0, j = 0;
    for (; i < ni && j < nj; i++, j++) {
        cmp = (*cmp_items)(kitems + i, citems + j);

        if (lalr && !cmp) {
            struct lr_item const *last = kitems + i;
            while (i < ni - 1 && !(*cmp_items)(last, kitems + i + 1)) i++;
            while (j < nj - 1 && !(*cmp_items)(last, citems + j + 1)) j++;
        } else break;
    }

    if (i < ni && j >= nj) cmp = 1;
    else if (i >= ni && j < nj) cmp = -1;

    ncompares++;

    if (debug_is("gram_states_cmp")) {
        print_lr_itemset_compact(stderr, kernel);
        fprintf(stderr, " `cmp` ");
        print_lr_itemset_compact(stderr, itemset);
        fprintf(stderr, " = %d\n", cmp);
    }

    return cmp;
}
#undef KERN
#undef NONKERN

static int compare_lr0_itemsets(void const *a, void const *b) {
    return compare_itemsets(compare_lr0_items, false, a, b);
}

static int compare_lalr_itemsets(void const *a, void const *b) {
    return compare_itemsets(compare_lr0_items, true, a, b);
}

static int compare_lr1_itemsets(void const *a, void const *b) {
    return compare_itemsets(compare_lr1_items, false, a, b);
}

static void sort_itemset(struct lr_itemset *itemset) {
    qsort(itemset->items, itemset->nitems, sizeof (struct lr_item), compare_lr1_items);
}

bool lr_itemset_sorted(struct lr_itemset const *itemset) {
    for (unsigned i = 1; i < itemset->nitems; i++) {
        if (compare_lr1_items(itemset->items + (i - 1), itemset->items + i) > 0)
            return false;
    }

    return true;
}

static unsigned items_key(gram_sym_no nonterm, gram_sym_no const term, struct gram_stats const stats) {
    nonterm = nonterm - stats.terms - 1;
    return term ? nonterm * stats.terms + term - 1 : nonterm;
}

#define CLOSE(name) \
static struct lr_item *name( \
    unsigned *nitems, struct lr_item *items, gram_sym_no nt, gram_sym_no la, \
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec, \
    struct states_context *context \
)

#define CLOSE_ITEM(name) \
static struct lr_item *name( \
    unsigned *nitems, struct lr_item *items, gram_sym_no *nt, gram_sym_no la, \
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec, \
    struct states_context *context \
)

CLOSE(close);
CLOSE_ITEM(close_item);

static struct lr_item *with_lookahead(
    unsigned *nitems, struct lr_item *items, gram_sym_no *nt, gram_sym_no la,
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    gram_sym_no *n = nt + 1;

    while (*n) {
        struct bsiter it = bsiter(san->firsts[*n]);

        gram_sym_no l;
        while (bsnext(&l, &it))
            items = close(nitems, items, *nt, l, san, spec, context);

        if (!san->nullable[*n]) break;

        n++;
    }

    if (!*n)
        items = close(nitems, items, *nt, la, san, spec, context);

    return items;
}

CLOSE(close) {
    unsigned const key = items_key(nt, la, spec->stats);

    if (bselem(key, context->symset)) return items;
    bsins(key, context->symset);

    struct gram_symbol sym = spec->symbols[nt];

    gram_rule_no *r = sym.derives;
    while (*r) {
        gram_sym_no *s = spec->rules[*r];

        if (items) {
            *items++ = (struct lr_item) { *r, 0, la };
        }

        (*nitems)++;

        if (*s < NONTERM0(spec->stats)) {
            r++; continue;
        }

        items = close_item(nitems, items, s, la, san, spec, context);
        r++;
    }

    return items;
}

CLOSE_ITEM(close_item) {
    if (context->item_type != GM_LR0_ITEMS) {
        return with_lookahead(nitems, items, nt, la, san, spec, context);
    } else {
        return close(nitems, items, *nt, la, san, spec, context);
    }
}

static unsigned closure(
    struct lr_itemset *kernel, struct lr_item const *item,
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    assert(kernel || item);
    assert(san != NULL);
    assert(spec != NULL);

    unsigned nitems = 0;

    if (item) {
        gram_sym_no *s = &spec->rules[item->rule][item->pos];
        if (*s >= NONTERM0(spec->stats))
            close_item(&nitems, NULL, s, item->sym, san, spec, context);
        return nitems + 1;
    }

    mdebug("kernel: "); debug_itemset(kernel); mdebug("\n");

    struct lr_item *items = &kernel->items[kernel->kitems];

    for (unsigned i = 0; i < kernel->kitems; i++) {
        struct lr_item kitem = kernel->items[i];
        gram_sym_no *s = &spec->rules[kitem.rule][kitem.pos];
        if (*s >= NONTERM0(spec->stats))
            items = close_item(&nitems, items, s, kitem.sym, san, spec, context);
    }

    kernel->nitems += nitems;

    sort_itemset(kernel);

    mdebug("nitems: %u, kitems: %u, closure: ", kernel->nitems, kernel->kitems); debug_itemset(kernel); mdebug("\n");

    return kernel->nitems;
}

static unsigned count_goto_items(
    gram_sym_no s, struct lr_itemset *itemset,
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    unsigned nitems = 0;

    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
        if (spec->rules[item.rule][item.pos] == s) {
            item.pos++;
            // count
            nitems += closure(NULL, &item, san, spec, context);
        }
    }

    bszero(context->symset);

    return nitems;
}

static void unify(struct lr_itemset *newset, struct lr_itemset *kernel, struct lr_itemset *itemset) {
    struct lr_item *nitem = newset->items;

    unsigned i;
    i = 0; while (i < itemset->kitems) *nitem++ = itemset->items[i++];
    i = 0; while (i < kernel->kitems) *nitem++ = kernel->items[i++];

    sort_itemset(newset);
}

static unsigned diff_itemset(struct lr_itemset *kernel, struct lr_itemset *itemset) {
    for (unsigned m = 0, s = 0; m < kernel->kitems && s < itemset->kitems;) {
        struct lr_item *kitem = kernel->items + m, *citem = itemset->items + s;

        int cmp = compare_lr1_items(kitem, citem);

        if (cmp < 0) m++;
        else if (cmp > 0) s++;
        else {
            *kitem = (struct lr_item) { 0 };
            memmove(kitem, kitem + 1, (kernel->kitems - (m + 1)) * sizeof *kitem);
            kernel->nitems--;
            kernel->kitems--;
            s++;
        }
    }

    return kernel->kitems;
}

#define DISCOVER_TRANSITIONS(name) \
static void name( \
    struct lr_transitions *trans, struct lr_itemset **itemset, gram_sym_no nsymbols, \
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec, \
    struct states_context *context \
)
#define DISCOVER_STATES(name) \
static void name( \
    struct lr_state **state, gram_sym_no sym, struct lr_itemset *kernel, \
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec, \
    struct states_context *context \
)

DISCOVER_TRANSITIONS(discover_transitions);
DISCOVER_STATES(discover_states);

static void merge_itemsets(
    struct rb_node *snode, struct lr_itemset *kernel,
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    struct lr_state *state = rbval(snode);
    struct lr_itemset *itemset = state->itemset;

    if (debug_is("gram_states_merge")) {
        fprintf(stderr, "merging itemsets:\n");
        print_lr_itemset_compact(stderr, kernel); fprintf(stderr, "\n");
        print_lr_itemset_compact(stderr, itemset); fprintf(stderr, "\n");
    }

    context->stats.diffs++;

    if (!diff_itemset(kernel, itemset)) {
        free(kernel);
        return;
    }

    context->stats.merges++;

    closure(kernel, NULL, san, spec, context);
    bszero(context->symset);

    unsigned nitems = itemset->nitems + kernel->nitems,
             kitems = itemset->kitems + kernel->kitems;

    context->stats.itemsets++;
    struct lr_itemset *newset = make_itemset(nitems, kitems, NULL);

    unify(newset, kernel, itemset);

    closure(newset, NULL, san, spec, context);
    bszero(context->symset);

    // normally this would be ill-advised, but all three of these sets are considered to be "equal"
    snode->assoc.key = newset;
    state->itemset = newset;

    struct lr_transitions const *trans = state->trans;

    // FIXME: would this work if I didn't modify the new kernel to remove existing items above?
    if (trans->nstates) {
        // FIXME: maxsym probably isn't needed. I should just be able to initialize the new states
        //        like normal
        gram_sym_no maxsym = trans->states[trans->nstates - 1]->sym;
        discover_transitions(NULL, &kernel, maxsym, san, spec, context);
    }

    free(kernel);
    free(itemset);
}

static struct lr_itemset *
goto_(
    gram_sym_no s, struct lr_itemset *itemset,
    struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec,
    struct states_context *context
) {
    unsigned nitems = count_goto_items(s, itemset, san, spec, context);
    if (!nitems) return NULL;

    context->stats.itemsets++;
    struct lr_itemset *kernel = make_itemset(nitems, 0, NULL);

    struct lr_item *kitem = kernel->items;
    for (unsigned i = 0; i < itemset->nitems; i++) {
        struct lr_item item = itemset->items[i];
        if (spec->rules[item.rule][item.pos] == s) {
            item.pos++;
            *kitem++ = item;
            kernel->nitems++;
            kernel->kitems++;
        }
    }

    sort_itemset(kernel);

    return kernel;
}

DISCOVER_TRANSITIONS(discover_transitions) {
    struct lr_itemset *kernel = NULL;

    for (gram_sym_no s = GM_SYMBOL0; s < offs(nsymbols); s++) {
        if ((kernel = goto_(s, *itemset, san, spec, context))) {
            struct lr_state *state;
            discover_states(trans ? trans->states + trans->nstates++ : &state, s, kernel, san, spec, context);
        }
    }
}

DISCOVER_STATES(discover_states) {
    int (*cmpfn)(void const *, void const *) = compare_lr0_itemsets;
    struct rb_node *snode = NULL;

    if (context->item_type == GM_LR1_ITEMS) cmpfn = compare_lr1_itemsets;
    else if (context->item_type == GM_LALR_ITEMS) cmpfn = compare_lalr_itemsets;

    context->stats.finds++;
    if ((snode = rbfind(kernel, cmpfn, context->states))) {
        *state = rbval(snode);

        if (context->item_type == GM_LALR_ITEMS) {
            merge_itemsets(snode, kernel, san, spec, context);
        } else {
            free(kernel);
        }

        return;
    }

    // close
    closure(kernel, NULL, san, spec, context);
    bszero(context->symset);

    struct lr_itemset *itemset = kernel;

    unsigned nstates = count_transitions(itemset, spec, context);
    struct lr_transitions *trans = make_transitions(nstates, 0, NULL);
    *state = make_state(context->nstates++, sym, itemset, trans);

    // this must happen prior to recursive calls to this function
    context->stats.inserts++;
    context->states = rbinsert(itemset, cmpfn, *state, context->states);

    discover_transitions(trans, &(*state)->itemset, spec->stats.symbols, san, spec, context);
}

struct lr_state *
discover_lr_states(unsigned *nstates, enum lr_item_type item_type, struct gram_symbol_analysis const *san, struct gram_parser_spec const *spec) {
    struct states_context context = { 0 };

    if (!states_context(&context, item_type, spec->stats)) return NULL;

    struct lr_item item0 = { GM_START };
    if (item_type != GM_LR0_ITEMS) item0.sym = GM_EOF;

    // count
    unsigned nitems = closure(NULL, &item0, san, spec, &context);
    bszero(context.symset);

    context.stats.itemsets++;
    struct lr_itemset *kernel = make_itemset(nitems, 1, &item0);
    if (!kernel) return free_states_context(&context), NULL;

    struct lr_state *states;
    discover_states(&states, 0, kernel, san, spec, &context);
    *nstates = context.nstates;

    if (debug_is("gram_states_stats")) {
        print_stats(stderr, &context);
    }

    if (debug_is("gram_states")) {
        fprintf(stderr, "state tree:\n");
        print_rbtree(stderr, _print_itemset_compact, context.states);
    }

    free_states_context(&context);

    return states;
}

void free_lr_states(unsigned nstates, struct lr_state *state) {
    if (!state) return;

    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return;
    struct lr_state **states = calloc(nstates, sizeof *states);
    if (!states) return free_array(stack);

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

void print_lr_states(FILE *handle, unsigned nstates, struct lr_state *state, struct gram_parser_spec const *spec) {
    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return;
    bool *visited = calloc(nstates, sizeof *visited);
    if (!visited) return free_array(stack);

    fprintf(handle, "states:\n");

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (visited[state->num]) continue;
        visited[state->num] = true;

        fprintf(handle, "\n");
        print_lr_state(handle, state, spec);

        struct lr_transitions *trans = state->trans;
        for (unsigned i = trans->nstates; i > 0; i--) {
            apush(&trans->states[i - 1], stack);
        }
    }

    free_array(stack);
    free(visited);
}

static void state_to_gvnode(char *buf, struct gram_parser_spec const *spec, Agnode_t **nodes, Agraph_t *graph, struct lr_state *state, Agnode_t *parent) {
    Agnode_t *node = nodes[state->num];

    if (!node) {
        sprintf(buf, "n%u", state->num);
        nodes[state->num] = node = agnode(graph, buf, 1);

        char *bufp = buf;
        bufp += sprintf(bufp, "<table cellspacing=\"0\" cellpadding=\"6\" border=\"0\"><tr><td border=\"1\" color=\"#cccccc\">#%u </td></tr>", state->num);

        struct lr_itemset *itemset = state->itemset;
        for (unsigned i = 0; i < itemset->nitems; i++) {
            struct lr_item item = itemset->items[i];
            bool reduction = !spec->rules[item.rule][item.pos];
            bufp += sprintf(bufp, "<tr><td align=\"left\" border=\"1\" bgcolor=\"%s\">%s",
                    reduction ? "#c72804" : "#ffffff",
                    reduction ? "<font color=\"#ffffff\">" : "");
            char *fmt = "(%u, %u) ";
            if (item.sym) fmt = "(%u, %u, %u) ";
            bufp += sprintf(bufp, fmt, item.rule, item.pos, item.sym);
            bufp += sprintf(bufp, "%s</td></tr>", reduction ? "</font>" : "");
        }

        sprintf(bufp, "</table>");

        agstrdup_html(graph, buf);
        agset(node, "label", buf);

        struct lr_transitions *trans = state->trans;
        for (unsigned i = trans->nstates; i > 0; i--) {
            state_to_gvnode(buf, spec, nodes, graph, trans->states[i - 1], node);
        }
    }

    if (parent) {
        sprintf(buf, "e%u-%u", state->num, state->sym);
        Agedge_t *edge = agedge(graph, parent, node, buf, 1);
        sprintf(buf, "%u", state->sym);
        agset(edge, "label", buf);
    }
}

bool print_lr_states_dot(FILE *handle, unsigned nstates, struct lr_state *state, struct gram_parser_spec const *spec) {
    Agnode_t **nodes = calloc(nstates, sizeof *nodes);
    if (!nodes) return false;
    char buf[1 << 19] = "";

    Agraph_t *graph = agopen("top", Agdirected, NULL);

    default_styles(graph);
    agattr(graph, AGNODE, "shape", "none");
    agattr(graph, AGNODE, "margin", "0");

    state_to_gvnode(buf, spec, nodes, graph, state, NULL);

    if (agwrite(graph, handle) == EOF) {
        return fprintf(stderr, "writing dot file failed\n"), false;
    }

    agclose(graph);
    free(nodes);

    return true;
}
