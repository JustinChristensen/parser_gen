#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "parser.h"
#include "nfa.h"

struct nfa_context nfa_context(struct nfa_state *statebuf) {
    return (struct nfa_context) {
        .statebuf = statebuf,
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

struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right) {
    return (struct nfa_state) {
        .type = BRANCH_STATE,
        .left = left,
        .right = right
    };
}

struct nfa_state symbol_state(struct nfa_state *next, char symbol) {
    return (struct nfa_state) {
        .type = SYMBOL_STATE,
        .symbol = symbol,
        .next = next
    };
}

struct nfa_state *setst(struct nfa_context *context, struct nfa_state state) {
    *context->statebuf = state;
    return context->statebuf++;
}

void smachine(struct nfa_context *context, struct nfa_machine machine) {
    context->nfa = machine;
}

struct nfa_machine gmachine(struct nfa_context *context) {
    return context->nfa;
}

struct nfa_machine empty_machine(struct nfa_context *context) {
    struct nfa_machine machine;
#ifdef DEBUG
    printf("empty machine\n");
#endif
    machine.end = setst(context, accepting_state());
    machine.start = setst(context, epsilon_state(machine.end));
    return machine;
}

struct nfa_machine symbol_machine(struct nfa_context *context, char symbol) {
    struct nfa_machine machine;
#ifdef DEBUG
    printf("symbol machine: %c\n", symbol);
#endif
    machine.end = setst(context, accepting_state());
    machine.start = setst(context, symbol_state(machine.end, symbol));
    return machine;
}

struct nfa_machine alt_machine(struct nfa_context *context, struct nfa_machine left, struct nfa_machine right) {
    struct nfa_machine machine;
#ifdef DEBUG
    printf("alt machine\n");
#endif
    machine.end = setst(context, accepting_state());
    *left.end = epsilon_state(machine.end);
    *right.end = epsilon_state(machine.end);
    machine.start = setst(context, branch_state(left.start, right.start));
    return machine;
}

struct nfa_machine cat_machine(struct nfa_machine first, struct nfa_machine second) {
    *first.end = *second.start;
#ifdef DEBUG
    printf("cat machine\n");
#endif

    return (struct nfa_machine) {
        .start = first.start,
        .end = second.end
    };
}

struct nfa_machine closure_machine(struct nfa_context *context, struct nfa_machine inner) {
    struct nfa_machine machine;
#ifdef DEBUG
    printf("closure machine\n");
#endif
    machine.end = setst(context, accepting_state());
    *inner.end = branch_state(inner.start, machine.end);
    machine.start = setst(context, branch_state(inner.start, machine.end));
    return machine;
}

bool nfa_from_regex(struct parse_context *context) {
    if (nfa_from_expr(context) && expect(context, '\0', NULL)) {
        return true;
    }

    return false;
}

bool nfa_from_expr(struct parse_context *context) {
    struct nfa_context *nfa_context = context->result_context;
    smachine(nfa_context, empty_machine(nfa_context));
    nfa_from_alt(context, gmachine(nfa_context));
    return true;
}

bool nfa_from_alt(struct parse_context *context, struct nfa_machine lmachine) {
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

bool nfa_from_cat(struct parse_context *context, struct nfa_machine lmachine) {
    struct nfa_context *nfa_context = context->result_context;

    if (nfa_from_factor(context)) {
        while (true) {
            lmachine = gmachine(nfa_context);

            if (nfa_from_factor(context)) {
                smachine(nfa_context, cat_machine(lmachine, gmachine(nfa_context)));
                continue;
            }

            break;
        }

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
        // smachine(nfa_context, cat_machine(lmachine, gmachine(nfa_context)));
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
        struct nfa_machine lmachine = gmachine(context);
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

bool nfa_match(char *str, struct nfa_context *context) {
    return true;
}

void print_state_table(struct nfa_state *start, struct nfa_state *end) {
    while (start != end) {
        printf("%p: ", start);
        switch (start->type) {
            case ACCEPTING_STATE:
                printf("accept");
                break;
            case EPSILON_STATE:
                printf("empty %p", start->next);
                break;
            case BRANCH_STATE:
                printf("branch %p %p", start->left, start->right);
                break;
            case SYMBOL_STATE:
                printf("symbol %p %c", start->next, start->symbol);
                break;
        }
        printf("\n");
        start++;
    }
}

