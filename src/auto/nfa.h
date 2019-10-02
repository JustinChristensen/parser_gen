#ifndef AUTO_NFA_H_
#define AUTO_NFA_H_ 1

/**
 * Data Structure Options:
 *
 * edge-labeled graph (adjacency list) where
 *  verticies are numbered states containing a list of at most 2 transitions
 *  edges are transitions, labeled by epsilon or the transition symbol, to another vertex
 *
 * edge-labeled graph (adjacency matrix) where
 *  struct transition graph[MAX_STATES][MAX_STATES]
 *  vertecies are numbered states where the number is an index
 *  edges contain the index of the state pointed to
 */

#include <stdbool.h>
#include "parser.h"

#define STATE_MAX 10000

enum nfa_state_type {
    ACCEPTING_STATE,
    EPSILON_STATE,
    BRANCH_STATE,
    SYMBOL_STATE
};

struct nfa_state {
    enum nfa_state_type type;
    union {
        // epsilon, symbol
        struct { struct nfa_state *next; char symbol; };
        // branch
        struct { struct nfa_state *left; struct nfa_state *right; };
        // accepting
    };
};

struct nfa {
    struct nfa_state *start;
    struct nfa_state **end;
    struct nfa_state **end1;
};

struct nfa_error {
    struct parse_error perror;
};

struct nfa_context {
    struct nfa_state *statebuf;
    struct nfa nfa;
    bool has_error;
    struct nfa_error error;
};

struct nfa_context nfa_context(struct nfa_state *statebuf);
struct nfa_state accepting_state();
struct nfa_state epsilon_state(struct nfa_state *next);
struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right);
struct nfa_state symbol_state(char symbol);
struct nfa_state *setst(struct nfa_context *context, struct nfa_state state);

void point(struct nfa *machine, struct nfa_state **end, struct nfa_state **end1);
void patch(struct nfa machine, struct nfa_state *state);
void smachine(struct nfa_context *context, struct nfa machine);
struct nfa gmachine(struct nfa_context *context);
struct nfa empty_machine(struct nfa_context *context);
struct nfa symbol_machine(struct nfa_context *context, char symbol);
struct nfa alt_machine(struct nfa_context *context, struct nfa left, struct nfa right);
struct nfa cat_machine(struct nfa first, struct nfa second);
struct nfa closure_machine(struct nfa_context *context, struct nfa inner);
bool nfa_from_regex(struct parse_context *context);
bool nfa_from_expr(struct parse_context *context);
bool nfa_from_alt(struct parse_context *context, struct nfa lmachine);
bool nfa_from_cat(struct parse_context *context, struct nfa lmachine);
bool nfa_from_factor(struct parse_context *context);
struct nfa_context *nfa_regex(char *regex, struct nfa_context *context);
bool has_nfa_error(struct nfa_context *context);
struct nfa_error nfa_error(struct nfa_context *context);
void print_nfa_error(struct nfa_error error);
void free_nfa_context(struct nfa_context *context);
bool nfa_match(char *str, struct nfa_context *context);

void print_state_table(struct nfa_state *start, struct nfa_state *end);

#endif // AUTO_NFA_H_ 1
