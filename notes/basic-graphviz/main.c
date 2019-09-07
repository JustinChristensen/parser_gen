#include <stdlib.h>
#include <stdio.h>
#include <cgraph.h>

int main(int argc, char *argv[]) {
    Agraph_t *graph = agopen("test", Agundirected, NULL);
    Agnode_t *node1 = agnode(graph, "Node 1", 1);
    Agnode_t *node2 = agnode(graph, "Node 2", 1);
    // Agedge_t *edge1 = agedge(graph, node1, node2, "Edge 1", 1);
    agedge(graph, node1, node2, "Edge 1", 1);
    agattr(graph, AGNODE, "shape", "circle");
    agattr(graph, AGNODE, "color", "blue");
    agattr(graph, AGEDGE, "color", "red");
    agset(node1, "shape", "box");
    agset(node2, "color", "pink");
    if (agwrite(graph, stdout) == EOF) {
        fprintf(stderr, "writing dot file failed\n");
    }
    agclose(graph);
    return EXIT_SUCCESS;
}
