#include <cgraph.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "base/graphviz.h"
#include "base/string.h"

char *left_justify(char *str) {
    struct sizenl sz = numlines(str);
    char *newstr = calloc(sizeof *newstr, sz.sl + sz.nl + 1);

    char *c = str,
         *d = newstr;
    while (*c != '\0') {
        if (*c == '\n') {
            *d++ = '\\';
            *d++ = 'l';
        } else {
            *d++ = *c;
        }
        c++;
    }

    return newstr;
}

void default_styles(Agraph_t *graph) {
    agattr(graph, AGRAPH, "pad", "0.4,0.3");
    agattr(graph, AGNODE, "label", "\\N");
    agattr(graph, AGNODE, "style", "dashed");
    agattr(graph, AGNODE, "color", "#aaaaaa");
    agattr(graph, AGNODE, "shape", "box");
    agattr(graph, AGNODE, "fontname", "verdana");
    agattr(graph, AGNODE, "fontsize", "10");
    agattr(graph, AGNODE, "fontcolor", "#222222");
    agattr(graph, AGEDGE, "label", "");
    agattr(graph, AGEDGE, "fontname", "verdana");
    agattr(graph, AGEDGE, "fontsize", "10");
    agattr(graph, AGEDGE, "fontcolor", "#aaaaaa");
    agattr(graph, AGEDGE, "style", "dashed");
    agattr(graph, AGEDGE, "color", "#aaaaaa");
}

void print_dot(FILE *handle, void *ast, char *input, void (*to_graph) (Agraph_t *graph, Agnode_t *parent, void *ast)) {
    Agraph_t *topg = agopen("top", Agundirected, NULL);
    Agraph_t *astg = agsubg(topg, "ast", 1);
    Agraph_t *inputg;

    default_styles(topg);

    if (input) {
        inputg = agsubg(topg, "input", 1);
    }

    (*to_graph)(astg, NULL, ast);

    if (input) {
        Agnode_t *inputn = agnode(inputg, "input", 1);
        agset(inputn, "fontname", "monospace");
        char *input_label = left_justify(input);
        agset(inputn, "label", input_label);
        free(input_label);
    }

    if (agwrite(topg, handle) == EOF) {
        fprintf(stderr, "printing dot file failed\n");
    }

    agclose(topg);
}

static int uid = 0;
#define NAMEBUFSIZE 16

Agnode_t *append_node(Agraph_t *graph, Agnode_t *parent, char *label, char *elabel) {
    static char namebuf[NAMEBUFSIZE];
    sprintf(namebuf, "n%d", uid++);
    Agnode_t *node = agnode(graph, namebuf, 1);
    agset(node, "label", label);
    if (parent) {
        Agedge_t *edge = agedge(graph, parent, node, NULL, 1);
        if (elabel) agset(edge, "label", elabel);
    }
    return node;
}

