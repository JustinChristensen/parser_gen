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


/* graphs */
// CGRAPH_API Agraph_t *agopen(char *name, Agdesc_t desc, Agdisc_t * disc);
// CGRAPH_API int agclose(Agraph_t * g);
// CGRAPH_API Agraph_t *agread(void *chan, Agdisc_t * disc);
// CGRAPH_API Agraph_t *agmemread(const char *cp);
// CGRAPH_API void agreadline(int);
// CGRAPH_API void agsetfile(char *);
// CGRAPH_API Agraph_t *agconcat(Agraph_t * g, void *chan, Agdisc_t * disc);
// CGRAPH_API int agwrite(Agraph_t * g, void *chan);
// CGRAPH_API int agisdirected(Agraph_t * g);
// CGRAPH_API int agisundirected(Agraph_t * g);
// CGRAPH_API int agisstrict(Agraph_t * g);
// CGRAPH_API int agissimple(Agraph_t * g);

/* nodes */
// CGRAPH_API Agnode_t *agnode(Agraph_t * g, char *name, int createflag);
// CGRAPH_API Agnode_t *agidnode(Agraph_t * g, IDTYPE id, int createflag);
// CGRAPH_API Agnode_t *agsubnode(Agraph_t * g, Agnode_t * n, int createflag);
// CGRAPH_API Agnode_t *agfstnode(Agraph_t * g);
// CGRAPH_API Agnode_t *agnxtnode(Agraph_t * g, Agnode_t * n);
// CGRAPH_API Agnode_t *aglstnode(Agraph_t * g);
// CGRAPH_API Agnode_t *agprvnode(Agraph_t * g, Agnode_t * n);
//
// CGRAPH_API Agsubnode_t *agsubrep(Agraph_t * g, Agnode_t * n);
// CGRAPH_API int agnodebefore(Agnode_t *u, Agnode_t *v); /* we have no shame */

/* edges */
// CGRAPH_API Agedge_t *agedge(Agraph_t * g, Agnode_t * t, Agnode_t * h,
// 			char *name, int createflag);
// CGRAPH_API Agedge_t *agidedge(Agraph_t * g, Agnode_t * t, Agnode_t * h,
//               IDTYPE id, int createflag);
// CGRAPH_API Agedge_t *agsubedge(Agraph_t * g, Agedge_t * e, int createflag);
// CGRAPH_API Agedge_t *agfstin(Agraph_t * g, Agnode_t * n);
// CGRAPH_API Agedge_t *agnxtin(Agraph_t * g, Agedge_t * e);
// CGRAPH_API Agedge_t *agfstout(Agraph_t * g, Agnode_t * n);
// CGRAPH_API Agedge_t *agnxtout(Agraph_t * g, Agedge_t * e);
// CGRAPH_API Agedge_t *agfstedge(Agraph_t * g, Agnode_t * n);
// CGRAPH_API Agedge_t *agnxtedge(Agraph_t * g, Agedge_t * e, Agnode_t * n);

/* generic */
// CGRAPH_API Agraph_t *agraphof(void* obj);
// CGRAPH_API Agraph_t *agroot(void* obj);
// CGRAPH_API int agcontains(Agraph_t *, void *);
// CGRAPH_API char *agnameof(void *);
// CGRAPH_API int agrelabel(void *obj, char *name);	/* scary */
// CGRAPH_API int agrelabel_node(Agnode_t * n, char *newname);
// CGRAPH_API int agdelete(Agraph_t * g, void *obj);
// CGRAPH_API long agdelsubg(Agraph_t * g, Agraph_t * sub);	/* could be agclose */
// CGRAPH_API int agdelnode(Agraph_t * g, Agnode_t * arg_n);
// CGRAPH_API int agdeledge(Agraph_t * g, Agedge_t * arg_e);
// CGRAPH_API int agobjkind(void *);


