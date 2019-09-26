#ifndef AUTO_DOT_H_
#define AUTO_DOT_H_ 1

#include <cgraph.h>
#include "ast.h"

void regex_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr);
void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr);

#endif // AUTO_DOT_H_

