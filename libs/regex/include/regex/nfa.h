#ifndef REGEX_NFA_H_
#define REGEX_NFA_H_ 1

#include <stdbool.h>
#include <regex/base.h>
#include <regex/result_types.h>
#include <regex/parser.h>

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
    bool has_error;
    struct regex_error error;
};

struct nfa_match {
    char *orig_input;
    char *input;
    struct regex_loc input_loc;
    char *match_start;
    struct regex_loc match_loc;
    struct nfa mach;
    int num_states;
    bool *already_on;
    struct nfa_state **currstates;
    struct nfa_state **nextstates;
};

// parser interface
extern struct regex_parse_interface const nfa_parse_iface;

// scanner construction
bool nfa_context(struct nfa_context *context, struct regex_pattern const *patterns);
bool nfa_regex(int sym, char *tag, char *pattern, struct nfa_context *context);
bool nfa_has_error(struct nfa_context *context);
struct regex_error nfa_error(struct nfa_context *context);
void free_nfa_context(struct nfa_context *context);

// scanning
bool nfa_start_match(char *input, struct nfa_match *match, struct nfa_context *context);
int nfa_match(struct nfa_match *match);
struct regex_loc nfa_match_loc(struct nfa_match *match);
void re_match_lexeme(char *lexeme, struct nfa_match *match);
void free_nfa_match(struct nfa_match *match);

#endif // REGEX_NFA_H_
