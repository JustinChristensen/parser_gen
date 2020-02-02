#include <cgraph.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <base/graphviz.h>
#include <regex/nfa.h>
#include "dot.h"

void regex_to_graph(Agraph_t *graph, Agnode_t *_, struct expr *expr) {
    expr_to_graph(graph, NULL, expr);
}

void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr) {
    if (!expr) return;

    char label[128] = "";

    switch (expr->type) {
        case NULL_EXPR:
            fprintf(stderr, "null expression encountered\n");
            break;
        case EMPTY_EXPR:
            append_node(graph, parent, "ε", NULL);
            break;
        case DOTALL_EXPR:
            append_node(graph, parent, ".", NULL);
            break;
        case ID_EXPR:
            sprintf(label, "{%s}", expr->id);
            append_node(graph, parent, label, NULL);
            break;
        case ALT_EXPR:
            parent = append_node(graph, parent, "|", NULL);
            expr_to_graph(graph, parent, expr->lexpr);
            expr_to_graph(graph, parent, expr->rexpr);
            break;
        case CAT_EXPR:
            parent = append_node(graph, parent, "+", NULL);
            expr_to_graph(graph, parent, expr->lexpr);
            expr_to_graph(graph, parent, expr->rexpr);
            break;
        case STAR_EXPR:
            parent = append_node(graph, parent, "*", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case PLUS_EXPR:
            parent = append_node(graph, parent, "+", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case OPTIONAL_EXPR:
            parent = append_node(graph, parent, "?", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case REPEAT_EXACT_EXPR:
            sprintf(label, "{%d}", expr->num);
            parent = append_node(graph, parent, label, NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case SUB_EXPR:
            parent = append_node(graph, parent, "()", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case CHAR_CLASS_EXPR:
            parent = append_node(graph, parent, "[]", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case NEG_CLASS_EXPR:
            parent = append_node(graph, parent, "[^]", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RANGE_EXPR:
            sprintf(label, "%c-%c", expr->range.start, expr->range.end);
            parent = append_node(graph, parent, label, NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case SYMBOL_EXPR:
            sprintf(label, "%c", expr->symbol);
            parent = append_node(graph, parent, label, NULL);
            break;
    }
}

void nfa_to_graph(struct nfa_state *state) {
    Agnode_t *nodes[STATE_MAX] = { NULL };

    Agraph_t *graph = agopen("top", Agdirected, NULL);

    default_styles(graph);

    agattr(graph, AGRAPH, "rankdir", "LR");

    nfa_state_to_graph(graph, nodes, state, NULL, NULL);

    if (agwrite(graph, stdout) == EOF) {
        fprintf(stderr, "printing dot file failed\n");
    }

    agclose(graph);
}

#define NAMEBUFSIZE 16

void nfa_state_to_graph(Agraph_t *graph, Agnode_t **nodes, struct nfa_state *to, Agnode_t *from, char *edge_label) {
    if (!nodes[to->id]) {
        static char namebuf[NAMEBUFSIZE] = "";
        char symbuf[2] = "";

        // create a node
        sprintf(namebuf, "n%d", to->id);
        Agnode_t *node = nodes[to->id] = agnode(graph, namebuf, 1);

        // set the node label
        sprintf(namebuf, "%d", to->id);
        agset(node, "label", namebuf);

        switch (to->type) {
            case ACCEPTING_STATE:
                agset(node, "color", "red");
                agset(node, "fontcolor", "red");
                break;
            case EPSILON_STATE:
                nfa_state_to_graph(graph, nodes, to->next, node, "ε");
                break;
            case BRANCH_STATE:
                agset(node, "color", "darkgreen");
                agset(node, "fontcolor", "darkgreen");
                nfa_state_to_graph(graph, nodes, to->left, node, "ε");
                nfa_state_to_graph(graph, nodes, to->right, node, "ε");
                break;
            case DOTALL_STATE:
                agset(node, "color", "pink");
                agset(node, "fontcolor", "pink");
                symbuf[0] = '.';
                nfa_state_to_graph(graph, nodes, to->next, node, symbuf);
                break;
            case SYMBOL_STATE:
                agset(node, "color", "blue");
                agset(node, "fontcolor", "blue");
                symbuf[0] = to->symbol;
                nfa_state_to_graph(graph, nodes, to->next, node, symbuf);
                break;
        }
    }

    // make an arrow
    if (from) {
        Agedge_t *edge = agedge(graph, from, nodes[to->id], NULL, 1);
        agset(edge, "label", edge_label);
    }
}
