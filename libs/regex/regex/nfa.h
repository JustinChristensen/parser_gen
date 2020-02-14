#ifndef REGEX_NFA_H_
#define REGEX_NFA_H_ 1

/**
 * Data Structure Options:
 *
 * edge-labeled graph (adjacency list) where
 *  verticies are numbered states containing a list of at most 2 transitions
 *  edges are transitions, labeled by epsilon or the transition symbol, to another vertex
 *
 * edge-labeled graph (adjacency matrix) where
 *  struct transition graph[MAX_STATES][MAX_STATES]
 *  verticies are numbered states where the number is an index
 *  edges contain the index of the state dangleed to
 */

#include <stdbool.h>
#include <limits.h>
#include <base/list.h>
#include "result_types.h"
#include "base.h"
#include "parser.h"

#define POOL_SIZE 1000
#define CLASS_SIZE (UCHAR_MAX + 1)

struct nfa_state_pool {
    struct nfa_state states[POOL_SIZE];
    int n;
    struct nfa_state_pool *next;
};

struct nfa_context {
    struct nfa_state_pool *state_pools;
    struct nfa_state_pool *state_pool;
    int numstates;
    struct nfa nfa;
    // TODO: think about linking existing NFAs
    // maybe I've only got one accepting state, and then a state
    // that denotes the action to set the current sym?
    // mach #1 -> set sym -> accept
    // mach #2 -> set sym -> accept
    // mach #3 = a(mach #1 -> set sym)b
    // where "set sym" is a specialized epsilon state?
    // struct hash_table *tagged_nfas;
    bool *current_class;
    bool use_nonrec;
    bool has_error;
    struct regex_error error;
};

struct nfa_match {
    struct nfa_context *context;
    char *input;
    struct regex_loc input_loc;
    char *match_start;
    struct regex_loc match_loc;
};

// nfa state constructors
struct nfa_state accepting_state();
struct nfa_state epsilon_state(struct nfa_state *next);
struct nfa_state dotall_state(struct nfa_state *next);
struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right);
struct nfa_state class_state(bool *char_class);
struct nfa_state symbol_state(char symbol);

// nfa context
struct nfa_context nfa_context(
    struct regex_pattern const *patterns,
    bool use_nonrec
);
struct nfa_state *setst(struct nfa_state states, truct nfa_context *context);
void dangle(struct nfa *machine, struct nfa_state **end, struct nfa_state **end1);
void point(struct nfa machine, struct nfa_state *state);
void smachine(struct nfa_context *context, struct nfa machine);
struct nfa gmachine(struct nfa_context *context);

// nfa machine constructors
union regex_result nfa_to_result(struct nfa_context *context);
struct nfa empty_machine(struct nfa_context *context);
struct nfa symbol_machine(struct nfa_context *context, char symbol);
struct nfa class_machine(struct nfa_context *context, bool *char_class);
struct nfa alt_machine(struct nfa_context *context, struct nfa left, struct nfa right);
struct nfa cat_machine(struct nfa first, struct nfa second);
struct nfa closure_machine(struct nfa_context *context, struct nfa inner);

// construct and simulate an nfa
void nfa_regex(int sym, char *tag, char *pattern, struct nfa_context *context);
bool nfa_has_error(struct nfa_context *context);
struct nfa_error nfa_error(struct nfa_context *context);
void free_nfa_context(struct nfa_context *context);
void eps_closure(struct list *nstates, struct nfa_state *state, bool *already_on);
void move(struct list *nstates, struct list *cstates, char c, bool *already_on);
bool accepts(struct list *cstates, struct nfa_state *accept);

struct nfa_match nfa_match_state(char *input, struct regex_loc loc, struct nfa_context *context);
int nfa_match(struct nfa_match *match);
struct regex_loc nfa_match_loc(struct nfa_match *match);
void nfa_match_lexeme(char *lexeme, struct nfa_match *match);

bool clone_machine(struct nfa mach, struct nfa_context *context);

// parse actions
bool noop_nfa(union regex_result _, struct nfa_context *context);
bool do_empty_nfa(union regex_result _, struct nfa_context *context);
bool do_alt_nfa(union regex_result nfa, struct nfa_context *context);
bool do_cat_nfa(union regex_result nfa, struct nfa_context *context);
bool do_dotall_nfa(union regex_result _, struct nfa_context *context);
bool do_symbol_nfa(union regex_result sym, struct nfa_context *context);
bool do_range_nfa(union regex_result range, struct nfa_context *context);
bool do_class_nfa(union regex_result _, struct nfa_context *context);
bool do_neg_class_nfa(union regex_result _, struct nfa_context *context);
bool do_star_nfa(union regex_result _, struct nfa_context *context);
bool do_plus_nfa(union regex_result _, struct nfa_context *context);
bool do_optional_nfa(union regex_result _, struct nfa_context *context);
bool do_repeat_exact_nfa(union regex_result num, struct nfa_context *context);

// parse action table
extern bool (*nfa_actions[NUM_ACTIONS])(union regex_result val, struct nfa_context *context);

#endif // REGEX_NFA_H_
