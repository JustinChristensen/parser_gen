#include <cgraph.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <base/btree.h>
#include <base/graphviz.h>
#include <base/ord.h>
#include <base/macros.h>
#include "btree_dot.h"

static bool red(struct bin *node) {
    return node && node->red;
}

static int uid = 0;

#define NAMEBUFSIZE 16
#define LABELBUFSIZE 64
Agnode_t *node_to_agnode(Agraph_t *graph, struct bin *node, void (*key_to_str)(char *out, void const *key)) {
    // determine the node id
    char namebuf[NAMEBUFSIZE];
    sprintf(namebuf, "n%d", uid++);

    // make the graph node
    Agnode_t *agn = agnode(graph, namebuf, 1);

    // determine the node label
    char labelbuf[NAMEBUFSIZE] = "nil";

    if (node) {
        (*key_to_str)(labelbuf, nodekey(node));
    }

    agset(agn, "label", labelbuf);

    // paint it
    if (red(node)) {
        agset(agn, "color", "#ff0000");
        agset(agn, "fontcolor", "#ff0000");
    }


    if (node) {
        bool with_nils = getenv("WITH_NILS") ? true : false;

        if (node->left || with_nils) {
            Agedge_t *ledge = agedge(graph, agn, node_to_agnode(graph, node->left, key_to_str), NULL, 1);
            if (red(node->left)) agset(ledge, "color", "#ff0000");
        }

        if (node->right || with_nils) {
            Agedge_t *redge = agedge(graph, agn, node_to_agnode(graph, node->right, key_to_str), NULL, 1);
            if (red(node->right)) agset(redge, "color", "#ff0000");
        }
    }

    return agn;
}

void btree_to_graph(struct bin *node, void (*key_to_str)(char *out, void const *key)) {
    Agraph_t *graph = agopen("tree", Agundirected, NULL);

    default_styles(graph);

    // start with black nodes and edges
    agattr(graph, AGNODE, "color", "#000000");
    agattr(graph, AGNODE, "fontcolor", "#000000");
    agattr(graph, AGEDGE, "color", "#000000");
    agattr(graph, AGEDGE, "fontcolor", "#000000");

    agattr(graph, AGRAPH, "rankdir", "TB");

    node_to_agnode(graph, node, key_to_str);

    if (agwrite(graph, stdout) == EOF) {
        fprintf(stderr, "printing dot file failed\n");
    }

    agclose(graph);
}

static void int_to_str(char *out, void const *key) {
    sprintf(out, "%d", *((int const *) key));
}

void graph_int_tree() {
    char *env_size = getenv("SIZE");
    struct bin *node = NULL;
    // int intlist[] = {82,61,28,49,76,13,25,31,43,58,85,1,10,52,37,34,7,73,19,64,4,91,22,46,67,55,79,16,88,40,70};
    int intlist[] = {10,37,76,25,19,67,85,34,31,82,91,49,16,61,88,64,79,70,55,43,1,22,28,40,52,58,46,73,4,7,13};
    size_t size = SIZEOF(intlist);
    int *keys = intlist;

    if (env_size) {
        size = atoi(env_size);
        keys = calloc(size, sizeof *keys);
        assert(keys != NULL);

        for (int i = 0; i < size; i += 3)
            keys[i] = i;
    }

    for (int i = 0; i < size; i++) {
        node = btinsert(&keys[i], CMPFN intcmp, NULL, node);
    }

    btree_to_graph(node, int_to_str);

    if (env_size) free(keys);
    free_btree(node);
}


