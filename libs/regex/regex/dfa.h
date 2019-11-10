#ifndef REGEX_DFA_H_
#define REGEX_DFA_H_ 1

#include <stdbool.h>
#include <base/intset.h>
#include "result_types.h"
#include "parser.h"

struct dfa_error {
    struct parse_error perror;
};

struct dfa_context {
    struct dfa_node *nodes; // indexed by id
    size_t n; // number of nodes
    struct dfa_node root; // ast decorated with position sets
    bool has_error;
    struct dfa_error error;
};

// dfa node constructors
struct dfa_node symbol_node(unsigned int id, char symbol, struct dfa_pos pos);
struct dfa_node empty_node(unsigned int id, struct dfa_pos pos);
struct dfa_node dotall_node(unsigned int id, struct dfa_pos pos);
struct dfa_node alt_node(unsigned int id, struct dfa_node *left, struct dfa_node *right, struct dfa_pos pos);
struct dfa_node cat_node(unsigned int id, struct dfa_node *left, struct dfa_node *right, struct dfa_pos pos);
struct dfa_node star_node(unsigned int id, struct dfa_node *node, struct dfa_pos pos);
struct dfa_node plus_node(unsigned int id, struct dfa_node *node, struct dfa_pos pos);
struct dfa_node optional_node(unsigned int id, struct dfa_node *node, struct dfa_pos pos);

void free_dfa_node(struct dfa_node *node);
void free_dfa_pos(struct dfa_pos *node);

struct dfa_context dfa_context(struct array *nodes);
void sdfanode(struct dfa_context *context, struct dfa_node node);
struct dfa_node gdfanode(struct dfa_context *context);
union rval dfa_node_to_rval(struct dfa_context *context);
void free_dfa_context(struct dfa_context *context);

// parse actions
void noop_dfa(struct dfa_context *context, union rval _);
void do_empty_dfa(struct dfa_context *context, union rval _);
void do_alt_dfa(struct dfa_context *context, union rval lnode);
void do_cat_dfa(struct nfa_context *context, union rval lnode);
void do_dotall_dfa(struct dfa_context *context, union rval _);
void do_symbol_dfa(struct dfa_context *context, union rval sym);
void do_star_dfa(struct dfa_context *context, union rval _);
void do_plus_dfa(struct dfa_context *context, union rval _);
void do_optional_dfa(struct nfa_context *context, union rval _);

void (*dfa_actions[NUMACTIONS])(void *context, union rval lval);

#endif // REGEX_DFA_H_
