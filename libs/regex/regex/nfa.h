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

#define STATE_POOL_SIZE 1000
#define CLASS_SIZE (UCHAR_MAX + 1)

enum {
    TAG_ONLY = -1,
    REJECTED = 0
};

struct nfa_state_pool {
    struct nfa_state states[STATE_POOL_SIZE];
    int n;
    struct nfa_state_pool *next;
};

struct tagged_nfa {
    struct tagged_nfa *next;
    char *tag;
    struct nfa nfa;
};

struct nfa_context {
    struct nfa_state_pool *state_pools;
    struct nfa_state_pool *state_pool;
    int num_states;
    struct tagged_nfa *tagged_nfas;
    struct nfa nfa;
    bool *current_class;
    bool use_nonrec;
    bool has_error;
    struct regex_error error;
};

struct nfa_match {
    struct nfa mach;
    int num_states;
    bool *already_on;
    struct nfa_state **currstates;
    struct nfa_state **nextstates;
    char *input;
    struct regex_loc input_loc;
    char *match_start;
    struct regex_loc match_loc;
};

// nfa state constructors
struct nfa_state accepting_state(int sym);
struct nfa_state epsilon_state(struct nfa_state *next);
struct nfa_state dotall_state(struct nfa_state *next);
struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right);
struct nfa_state class_state(bool *char_class);
struct nfa_state char_state(char ch);

// nfa context
bool nfa_context(struct regex_pattern const *patterns, bool use_nonrec, struct nfa_context *context);
struct nfa_state *setst(struct nfa_state state, struct nfa_context *context);
void dangle(struct nfa *machine, struct nfa_state **end, struct nfa_state **end1);
struct nfa_state **point(struct nfa machine, struct nfa_state *state);
struct nfa_state **merge(struct nfa machine, struct nfa_context *context);
void smachine(struct nfa machine, struct nfa_context *context);
struct nfa gmachine(struct nfa_context *context);

// nfa machine constructors
union regex_result nfa_to_result(struct nfa_context *context);
struct nfa empty_machine(struct nfa_context *context);
struct nfa char_machine(char ch, struct nfa_context *context);
struct nfa class_machine(bool *char_class, struct nfa_context *context);
struct nfa alt_machine(struct nfa left, struct nfa right, struct nfa_context *context);
struct nfa cat_machine(struct nfa first, struct nfa second);
struct nfa closure_machine(struct nfa inner, struct nfa_context *context);

// construct and simulate an nfa
bool nfa_regex(int sym, char *tag, char *pattern, struct nfa_context *context);
bool nfa_has_error(struct nfa_context *context);
struct regex_error nfa_error(struct nfa_context *context);
void free_nfa_context(struct nfa_context *context);
struct nfa_state **eps_closure(
    int *foundsym,
    struct nfa_state **nsp,
    bool *already_on,
    struct nfa_state *state
);
struct nfa_state **move(
    int *foundsym,
    struct nfa_state **nsp,
    struct nfa_state **cstates,
    struct nfa_state **csp,
    bool *already_on,
    char c
);

bool nfa_match_state(struct nfa_match *match, char *input, struct nfa_context *context);
int nfa_match(struct nfa_match *match);
struct regex_loc nfa_match_loc(struct nfa_match *match);
void nfa_match_lexeme(char *lexeme, struct nfa_match *match);
void free_nfa_match(struct nfa_match *match);

bool clone_machine(struct nfa mach, struct nfa_context *context);
struct tagged_nfa *find_machine(char *tag, struct nfa_context *context);
bool tag_machine(char *tag, struct nfa_context *context);

// parse actions
bool noop_nfa(union regex_result _, struct nfa_context *context);
bool do_regex_nfa(union regex_result _, struct nfa_context *context);
bool do_tag_nfa(union regex_result _, struct nfa_context *context);
bool do_empty_nfa(union regex_result _, struct nfa_context *context);
bool do_alt_nfa(union regex_result lhs, struct nfa_context *context);
bool do_cat_nfa(union regex_result lhs, struct nfa_context *context);
bool do_dotall_nfa(union regex_result _, struct nfa_context *context);
bool do_char_nfa(union regex_result ch, struct nfa_context *context);
bool do_range_nfa(union regex_result range, struct nfa_context *context);
bool do_class_nfa(union regex_result _, struct nfa_context *context);
bool do_neg_class_nfa(union regex_result _, struct nfa_context *context);
bool do_star_nfa(union regex_result _, struct nfa_context *context);
bool do_plus_nfa(union regex_result _, struct nfa_context *context);
bool do_optional_nfa(union regex_result _, struct nfa_context *context);
bool do_repeat_exact_nfa(union regex_result num, struct nfa_context *context);

// parse action table
extern bool (*nfa_actions[NUM_ACTIONS])(union regex_result val, void *context);
extern struct parse_interface nfa_pinterface;

#endif // REGEX_NFA_H_
