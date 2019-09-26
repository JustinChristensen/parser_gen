#include <cgraph.h>
#include <stdio.h>
#include <base/graphviz.h>
#include "dot.h"

void regex_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr) {
    expr_to_graph(graph, NULL, expr);
}

void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr) {
    char sym[2] = { 0 };

    switch (expr->type) {
        case NULL_EXPR:
            fprintf(stderr, "null expression encountered\n");
            break;
        case EMPTY_EXPR:
            append_node(graph, parent, "ε");
            break;
        case ALT_EXPR:
            parent = append_node(graph, parent, "|");
            expr_to_graph(graph, parent, expr->lexpr);
            expr_to_graph(graph, parent, expr->rexpr);
            break;
        case CAT_EXPR:
            parent = append_node(graph, parent, "+");
            expr_to_graph(graph, parent, expr->lexpr);
            expr_to_graph(graph, parent, expr->rexpr);
            break;
        case STAR_EXPR:
            parent = append_node(graph, parent, "*");
            expr_to_graph(graph, parent, expr->expr);
            break;
        case SUB_EXPR:
            parent = append_node(graph, parent, "()");
            expr_to_graph(graph, parent, expr->expr);
            break;
        case SYMBOL_EXPR:
            sym[0] = expr->symbol;
            parent = append_node(graph, parent, sym);
            break;
    }
}
