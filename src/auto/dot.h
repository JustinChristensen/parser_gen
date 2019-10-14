#ifndef AUTO_DOT_H_
#define AUTO_DOT_H_ 1

#include <cgraph.h>
#include <regex/ast.h>
#include <regex/nfa.h>

void regex_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr);
void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr);
void nfa_to_graph(struct nfa_state *state);
void nfa_state_to_graph(Agraph_t *graph, Agnode_t **nodes, struct nfa_state *to, Agnode_t *from, char *edge_label);

#endif // AUTO_DOT_H_

