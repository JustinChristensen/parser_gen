#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/list.h>
#include "parser.h"
#include "nfa.h"

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

int uid = 0;

struct nfa_state *setst(struct nfa_context *context, struct nfa_state state) {
    state.id = uid++;
    *context->statebuf = state;
    context->numstates++;
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

struct nfa empty_machine(struct nfa_context *context) {
    struct nfa machine;
#ifdef DEBUG
    printf("empty machine\n");
#endif
    machine.start = setst(context, epsilon_state(NULL));
    point(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa dotall_machine(struct nfa_context *context) {
    struct nfa machine;
#ifdef DEBUG
    printf("dotall machine\n");
#endif
    machine.start = setst(context, dotall_state(NULL));
    point(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa symbol_machine(struct nfa_context *context, char symbol) {
    struct nfa machine;
#ifdef DEBUG
    printf("symbol machine: %c\n", symbol);
#endif
    machine.start = setst(context, symbol_state(symbol));
    point(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa alt_machine(struct nfa_context *context, struct nfa left, struct nfa right) {
    struct nfa machine;
#ifdef DEBUG
    printf("alt machine\n");
#endif
    machine.start = setst(context, branch_state(left.start, right.start));
    patch(left, setst(context, epsilon_state(NULL)));
    patch(right, setst(context, epsilon_state(NULL)));
    point(&machine, &(*left.end)->next, &(*right.end)->next);
    return machine;
}

struct nfa cat_machine(struct nfa first, struct nfa second) {
    struct nfa machine;
#ifdef DEBUG
    printf("cat machine\n");
#endif
    patch(first, second.start);
    machine.start = first.start;
    point(&machine, second.end, second.end1);
    return machine;
}

struct nfa posclosure_machine(struct nfa_context *context, struct nfa inner) {
    struct nfa machine;
#ifdef DEBUG
    printf("positive closure machine\n");
#endif
    machine.start = inner.start;
    patch(inner, setst(context, branch_state(inner.start, NULL)));
    point(&machine, &(*inner.end)->right, NULL);
    return machine;
}

struct nfa optional_machine(struct nfa_context *context, struct nfa inner) {
    struct nfa machine;
#ifdef DEBUG
    printf("optional machine\n");
#endif
    patch(inner, setst(context, epsilon_state(NULL)));
    machine.start = setst(context, branch_state(inner.start, NULL));
    point(&machine, &(*inner.end)->next, &machine.start->right);
    return machine;
}

struct nfa closure_machine(struct nfa_context *context, struct nfa inner) {
#ifdef DEBUG
    printf("closure machine\n");
#endif
    return optional_machine(context, posclosure_machine(context, inner));
}

bool nfa_from_regex(struct parse_context *context) {
    struct nfa_context *nfa_context = context->result_context;
    if (nfa_from_expr(context) && expect(context, '\0', NULL)) {
        patch(gmachine(nfa_context), setst(nfa_context, accepting_state()));
        return true;
    }

    return false;
}

bool nfa_from_expr(struct parse_context *context) {
    nfa_from_alt(context, gmachine(context->result_context));
    return true;
}

bool nfa_from_alt(struct parse_context *context, struct nfa lmachine) {
    struct nfa_context *nfa_context = context->result_context;

    if (nfa_from_cat(context, lmachine)) {
        while (true) {
            lmachine = gmachine(nfa_context);

            if (peek(context, ALT, NULL) &&
                expect(context, ALT, NULL) &&
                nfa_from_expr(context)) {
                smachine(nfa_context, alt_machine(nfa_context, lmachine, gmachine(nfa_context)));
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

bool nfa_from_cat(struct parse_context *context, struct nfa lmachine) {
    struct nfa_context *nfa_context = context->result_context;

    struct nfa empty = empty_machine(nfa_context);

    smachine(nfa_context, empty);

    if (nfa_from_factor(context)) {
        while (true) {
            lmachine = gmachine(nfa_context);

            if (nfa_from_factor(context)) {
                smachine(nfa_context, cat_machine(lmachine, gmachine(nfa_context)));
                continue;
            }

            break;
        }

        smachine(nfa_context, cat_machine(empty, gmachine(nfa_context)));

        return true;
    }

    return false;
}

bool nfa_from_factor(struct parse_context *context) {
    struct nfa_context *nfa_context = context->result_context;
    bool has_head = false;

    if (peek(context, LPAREN, NULL) &&
        expect(context, LPAREN, NULL) &&
        nfa_from_expr(context) && expect(context, RPAREN, NULL)) {
        has_head = true;
    } else if (peek(context, DOTALL, NULL)) {
        expect(context, DOTALL, NULL);
        smachine(nfa_context, dotall_machine(nfa_context));
        has_head = true;
    } else if (peek(context, SYMBOL, is_symbol)) {
        int sym = lookahead(context);
        expect(context, SYMBOL, is_symbol);
        smachine(nfa_context, symbol_machine(nfa_context, (char) sym));
        has_head = true;
    }

    if (has_head) {
        while (true) {
            if (peek(context, STAR, NULL) && expect(context, STAR, NULL)) {
                smachine(nfa_context, closure_machine(nfa_context, gmachine(nfa_context)));
                continue;
            } else if (peek(context, PLUS, NULL) && expect(context, PLUS, NULL)) {
                smachine(nfa_context, posclosure_machine(nfa_context, gmachine(nfa_context)));
                continue;
            } else if (peek(context, OPTIONAL, NULL) && expect(context, OPTIONAL, NULL)) {
                smachine(nfa_context, optional_machine(nfa_context, gmachine(nfa_context)));
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

struct nfa_context *nfa_regex(char *regex, struct nfa_context *context) {
    if (!context) {
        struct nfa_state *statebuf = calloc(STATE_MAX, sizeof *statebuf);
        assert(statebuf != NULL);
        context = malloc(sizeof *context);
        *context = nfa_context(statebuf);
    }

    if (!has_nfa_error(context)) {
        struct nfa lmachine = gmachine(context);
        struct parse_context pcontext = parse_context(regex, context);

        if (!nfa_from_regex(&pcontext)) {
            context->has_error = pcontext.has_error;
            context->error = (struct nfa_error) { pcontext.error };
        } else if (lmachine.end) {
            smachine(context, alt_machine(context, lmachine, gmachine(context)));
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
    struct node *node;
    struct nfa_state *state;
    while ((node = pop(cstates)) && (state = value(node))) {
        if (state == accept) {
            return true;
        }

        free_node(node, NULL);
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

#ifdef DEBUG
    print_state_table(context->statebuf - numstates, context->statebuf);
    print_nfa_states(cstates);
#endif
    char c;
    struct list *t;
    while ((c = *str++) != '\0') {
        memset(already_on, false, numstates);
        move(nstates, cstates, c, already_on);
        t = cstates;
        cstates = nstates;
        nstates = t;
#ifdef DEBUG
        print_nfa_states(cstates);
#endif
    }

    bool result = accepts(cstates, *nfa.end);

    free(already_on);
    free_list(nstates, NULL);
    free_list(cstates, NULL);

    return result;
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
    printf("(%d, ", state->id);
    switch (state->type) {
        case ACCEPTING_STATE:
            printf("accept");
            break;
        case EPSILON_STATE:
            printf("eps, %p", state->next);
            break;
        case DOTALL_STATE:
            printf("dotall, %p", state->next);
            break;
        case BRANCH_STATE:
            printf("branch, %p, %p", state->left, state->right);
            break;
        case SYMBOL_STATE:
            printf("symbol, %p, %c", state->next, state->symbol);
            break;
    }
    printf(")");
}

void print_state_table(struct nfa_state *start, struct nfa_state *end) {
    while (start != end) {
        printf("%p: ", start);
        print_state(start);
        printf("\n");
        start++;
    }
}

