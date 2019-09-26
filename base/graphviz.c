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

void print_dot(FILE *handle, void *ast, char *input, void (*to_graph) (Agraph_t *graph, Agnode_t *parent, void *ast)) {
    Agraph_t *topg = agopen("top", Agundirected, NULL);
    Agraph_t *astg = agsubg(topg, "ast", 1);
    Agraph_t *inputg;

    if (input) {
        inputg = agsubg(topg, "input", 1);
    }

    agattr(topg, AGRAPH, "pad", "0.4,0.3");
    agattr(topg, AGNODE, "label", "\\N");
    agattr(topg, AGNODE, "style", "dashed");
    agattr(topg, AGNODE, "color", "#aaaaaa");
    agattr(topg, AGNODE, "shape", "box");
    agattr(topg, AGNODE, "fontname", "verdana");
    agattr(topg, AGNODE, "fontsize", "10");
    agattr(topg, AGNODE, "fontcolor", "#222222");
    agattr(topg, AGEDGE, "style", "dashed");
    agattr(topg, AGEDGE, "color", "#aaaaaa");

    (*to_graph)(astg, NULL, ast);

    if (input) {
        Agnode_t *inputn = agnode(inputg, "input", 1);
        agset(inputn, "fontname", "monospace");
        char *input_label = left_justify(input);
        agset(inputn, "label", input_label);
        free(input_label);
    }

    if (agwrite(topg, stdout) == EOF) {
        fprintf(stderr, "printing dot file failed\n");
    }

    agclose(topg);
}

static int uid = 0;
#define NAMEBUFSIZE 16

Agnode_t *append_node(Agraph_t *graph, Agnode_t *parent, char *label) {
    static char namebuf[NAMEBUFSIZE];
    sprintf(namebuf, "n%d", uid++);
    Agnode_t *node = agnode(graph, namebuf, 1);
    agset(node, "label", label);
    if (parent) agedge(graph, parent, node, NULL, 1);
    return node;
}

