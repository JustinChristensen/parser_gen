#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/list.h>
#include <base/debug.h>
#include "regex/nfa.h"
#include "regex/parser.h"
#include "regex/result_types.h"

void (*nfa_actions[NUMACTIONS])(void *context, union rval lval) = {
    [DO_REGEX] =    ACTION noop_nfa,
    [DO_EMPTY] =    ACTION do_empty_nfa,
    [DO_ALT] =      ACTION do_alt_nfa,
    [DO_CAT] =      ACTION do_cat_nfa,
    [DO_SUB] =      ACTION noop_nfa,
    [DO_DOTALL] =   ACTION do_dotall_nfa,
    [DO_SYMBOL] =   ACTION do_symbol_nfa,
    [DO_STAR] =     ACTION do_star_nfa,
    [DO_PLUS] =     ACTION do_plus_nfa,
    [DO_OPTIONAL] = ACTION do_optional_nfa
};

struct nfa_context nfa_context(struct nfa_state *statebuf) {
    return (struct nfa_context) {
        .statebuf = statebuf,
        .numstates = 0,
        .has_error = false,
        .error = { nullperr() }
    };
}

struct nfa_state accepting_state() {
    return (struct nfa_state) {
        .type = ACCEPTING_STATE
    };
}

struct nfa_state epsilon_state(struct nfa_state *next) {
    return (struct nfa_state) {
        .type = EPSILON_STATE,
        .next = next
    };
}

struct nfa_state dotall_state(struct nfa_state *next) {
    return (struct nfa_state) {
        .type = DOTALL_STATE,
        .next = next
    };
}

struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right) {
    return (struct nfa_state) {
        .type = BRANCH_STATE,
        .left = left,
        .right = right
    };
}

struct nfa_state symbol_state(char symbol) {
    return (struct nfa_state) {
        .type = SYMBOL_STATE,
        .symbol = symbol,
        .next = NULL
    };
}

struct nfa_state *setst(struct nfa_context *context, struct nfa_state state) {
    state.id = context->numstates++;
    *context->statebuf = state;
    return context->statebuf++;
}

void point(struct nfa *machine, struct nfa_state **end, struct nfa_state **end1) {
    machine->end = end;
    machine->end1 = end1;
}

void patch(struct nfa machine, struct nfa_state *state) {
    *machine.end = state;
    if (machine.end1) *machine.end1 = *machine.end;
}

void smachine(struct nfa_context *context, struct nfa machine) {
    context->nfa = machine;
}

struct nfa gmachine(struct nfa_context *context) {
    return context->nfa;
}

union rval nfa_to_rval(struct nfa_context *context) {
    return (union rval) { .mach = gmachine(context) };
}

struct nfa empty_machine(struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(context, epsilon_state(NULL));
    point(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa dotall_machine(struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(context, dotall_state(NULL));
    point(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa symbol_machine(struct nfa_context *context, char symbol) {
    struct nfa machine;
    machine.start = setst(context, symbol_state(symbol));
    point(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa alt_machine(struct nfa_context *context, struct nfa left, struct nfa right) {
    struct nfa machine;
    machine.start = setst(context, branch_state(left.start, right.start));
    patch(left, setst(context, epsilon_state(NULL)));
    patch(right, setst(context, epsilon_state(NULL)));
    point(&machine, &(*left.end)->next, &(*right.end)->next);
    return machine;
}

struct nfa cat_machine(struct nfa first, struct nfa second) {
    struct nfa machine;
    patch(first, second.start);
    machine.start = first.start;
    point(&machine, second.end, second.end1);
    return machine;
}

struct nfa posclosure_machine(struct nfa_context *context, struct nfa inner) {
    struct nfa machine;
    machine.start = inner.start;
    patch(inner, setst(context, branch_state(inner.start, NULL)));
    point(&machine, &(*inner.end)->right, NULL);
    return machine;
}

struct nfa optional_machine(struct nfa_context *context, struct nfa inner) {
    struct nfa machine;
    patch(inner, setst(context, epsilon_state(NULL)));
    machine.start = setst(context, branch_state(inner.start, NULL));
    point(&machine, &(*inner.end)->next, &machine.start->right);
    return machine;
}

struct nfa closure_machine(struct nfa_context *context, struct nfa inner) {
    return optional_machine(context, posclosure_machine(context, inner));
}

struct nfa_context *nfa_regex(char *regex, struct nfa_context *context) {
    assert(context != NULL);

    if (!has_nfa_error(context)) {
        struct nfa lmachine = gmachine(context);
        struct parse_context pcontext = parse_context(regex, context, GETVALFN nfa_to_rval, nfa_actions, NULL);

        // overwrite the previous accepting state if we're
        // chaining nfa_regex calls to create alt machines
        if (lmachine.end) {
            context->statebuf--;
            context->numstates--;
        }

        if (!parse_regex(&pcontext)) {
            context->has_error = pcontext.has_error;
            context->error = (struct nfa_error) { pcontext.error };
        } else {
            if (lmachine.end) do_alt_nfa(context, (union rval) lmachine);
            patch(gmachine(context), setst(context, accepting_state()));
        }
    }

    return context;
}

bool has_nfa_error(struct nfa_context *context) {
    return context->has_error;
}

struct nfa_error nfa_error(struct nfa_context *context) {
    return context->error;
}


void print_nfa_error(struct nfa_error error) {
    print_parse_error(error.perror);
}

void free_nfa_context(struct nfa_context *context) {
    free(context->statebuf);
    free(context);
}

void eps_closure(struct list *nstates, struct nfa_state *state, bool *already_on) {
    if (already_on[state->id]) return;

    push(nstates, state);
    already_on[state->id] = true;

    switch (state->type) {
        case EPSILON_STATE:
            eps_closure(nstates, state->next, already_on);
            break;
        case BRANCH_STATE:
            eps_closure(nstates, state->left, already_on);
            eps_closure(nstates, state->right, already_on);
            break;
        case ACCEPTING_STATE:
        case DOTALL_STATE:
        case SYMBOL_STATE:
            break;
    }
}

void move(struct list *nstates, struct list *cstates, char c, bool *already_on) {
    struct node *node;
    struct nfa_state *state;
    while ((node = pop(cstates)) && (state = value(node))) {
        if ((state->type == SYMBOL_STATE && state->symbol == c) || state->type == DOTALL_STATE) {
            eps_closure(nstates, state->next, already_on);
        }

        free_node(node, NULL);
    }
}

bool accepts(struct list *cstates, struct nfa_state *accept) {
    struct node *node, *next;
    struct nfa_state *state;

    for (node = head(cstates); node; node = next) {
        state = value(node);

        if (state == accept) {
            return true;
        }

        next = node->next;
    }

    return false;
}

bool nfa_match(char *str, struct nfa_context *context) {
    struct nfa nfa = context->nfa;
    size_t numstates = context->numstates;
    bool *already_on = calloc(numstates, sizeof *already_on);
    struct list *cstates = init_list();
    struct list *nstates = init_list();

    eps_closure(cstates, nfa.start, already_on);

    print_state_table(context->statebuf - numstates, context->statebuf);
    print_nfa_states(cstates);

    char c;
    struct list *t;
    while ((c = *str++) != '\0') {
        memset(already_on, false, numstates);
        move(nstates, cstates, c, already_on);
        t = cstates;
        cstates = nstates;
        nstates = t;
        print_nfa_states(cstates);
    }

    bool result = accepts(cstates, *nfa.end);

    free(already_on);
    free_list(nstates, NULL);
    free_list(cstates, NULL);

    return result;
}

void noop_nfa(struct nfa_context *context, union rval _) {}

void do_empty_nfa(struct nfa_context *context, union rval _) {
    smachine(context, empty_machine(context));
}

void do_alt_nfa(struct nfa_context *context, union rval lnfa) {
    smachine(context, alt_machine(context, lnfa.mach, gmachine(context)));
}

void do_cat_nfa(struct nfa_context *context, union rval lnfa) {
    smachine(context, cat_machine(lnfa.mach, gmachine(context)));
}

void do_dotall_nfa(struct nfa_context *context, union rval _) {
    smachine(context, dotall_machine(context));
}

void do_symbol_nfa(struct nfa_context *context, union rval sym) {
    smachine(context, symbol_machine(context, sym.sym));
}

void do_star_nfa(struct nfa_context *context, union rval _) {
    smachine(context, closure_machine(context, gmachine(context)));
}

void do_plus_nfa(struct nfa_context *context, union rval _) {
    smachine(context, posclosure_machine(context, gmachine(context)));
}

void do_optional_nfa(struct nfa_context *context, union rval _) {
    smachine(context, optional_machine(context, gmachine(context)));
}

void print_nfa_states(struct list *cstates) {
    struct node *node;

    if (!empty(cstates)) {
        printf("{");
        node = head(cstates);
        print_state(value(node));
        for (node = node->next; node; node = node->next) {
            printf(", ");
            print_state(value(node));
        }
        printf("}");
    } else {
        printf("empty");
    }

    printf("\n");
}

void print_state(struct nfa_state *state) {
    debug_(NULL, "(%d, ", state->id);
    switch (state->type) {
        case ACCEPTING_STATE:
            debug_(NULL, "accept");
            break;
        case EPSILON_STATE:
            debug_(NULL, "eps, %p", state->next);
            break;
        case DOTALL_STATE:
            debug_(NULL, "dotall, %p", state->next);
            break;
        case BRANCH_STATE:
            debug_(NULL, "branch, %p, %p", state->left, state->right);
            break;
        case SYMBOL_STATE:
            debug_(NULL, "symbol, %p, %c", state->next, state->symbol);
            break;
    }
    debug_(NULL, ")");
}

void print_state_table(struct nfa_state *start, struct nfa_state *end) {
    while (start != end) {
        debug_ns_("nfa", "%p: ", start);
        print_state(start);
        debug_(NULL, "\n");
        start++;
    }
}

