#ifndef BASE_GRAPHVIZ_H_
#define BASE_GRAPHVIZ_H_ 1

#include <cgraph.h>

#define TOGRAPHFN (void (*) (Agraph_t *, Agnode_t *, void *))

char *left_justify(char *str);
void print_dot(FILE *handle, void *ast, char *input, void (*to_graph) (Agraph_t *graph, Agnode_t *parent, void *ast));
Agnode_t *append_node(Agraph_t *graph, Agnode_t *parent, char *label);

#endif // BASE_GRAPHVIZ_H_
