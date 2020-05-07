#include <cgraph.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <base/graphviz.h>
#include <regex/nfa.h>
#include "dot.h"

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

void regex_to_graph(Agraph_t *graph, Agnode_t *_, struct regex_expr *expr) {
    expr_to_graph(graph, NULL, expr);
}

#pragma clang diagnostic pop

void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct regex_expr *expr) {
    if (!expr) return;

    char label[128] = "";

    switch (expr->type) {
        case RX_NULL_EXPR:
            fprintf(stderr, "null expression encountered\n");
            break;
        case RX_EMPTY_EXPR:
            append_node(graph, parent, "ε", NULL);
            break;
        case RX_DOTALL_EXPR:
            append_node(graph, parent, ".", NULL);
            break;
        case RX_TAG_EXPR:
            sprintf(label, "{%s}", expr->tag);
            append_node(graph, parent, label, NULL);
            break;
        case RX_ALT_EXPR:
            parent = append_node(graph, parent, "|", NULL);
            expr_to_graph(graph, parent, expr->lexpr);
            expr_to_graph(graph, parent, expr->rexpr);
            break;
        case RX_CAT_EXPR:
            parent = append_node(graph, parent, "+", NULL);
            expr_to_graph(graph, parent, expr->lexpr);
            expr_to_graph(graph, parent, expr->rexpr);
            break;
        case RX_STAR_EXPR:
            parent = append_node(graph, parent, "*", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_PLUS_EXPR:
            parent = append_node(graph, parent, "+", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_OPTIONAL_EXPR:
            parent = append_node(graph, parent, "?", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_REPEAT_EXACT_EXPR:
            sprintf(label, "{%d}", expr->num);
            parent = append_node(graph, parent, label, NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_SUB_EXPR:
            parent = append_node(graph, parent, "()", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_CHAR_CLASS_EXPR:
            parent = append_node(graph, parent, "[]", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_NEG_CLASS_EXPR:
            parent = append_node(graph, parent, "[^]", NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_RANGE_EXPR:
            sprintf(label, "%c-%c", expr->range.start, expr->range.end);
            parent = append_node(graph, parent, label, NULL);
            expr_to_graph(graph, parent, expr->expr);
            break;
        case RX_CHAR_EXPR:
            sprintf(label, "%c", expr->ch);
            parent = append_node(graph, parent, label, NULL);
            break;
    }
}

void nfa_to_graph(struct nfa_state *state, int num_states) {
    Agnode_t **nodes = calloc(num_states, sizeof *nodes);

    assert(nodes != NULL);

    Agraph_t *graph = agopen("top", Agdirected, NULL);

    default_styles(graph);

    agattr(graph, AGRAPH, "rankdir", "LR");

    nfa_state_to_graph(graph, nodes, state, NULL, NULL);

    if (agwrite(graph, stdout) == EOF) {
        fprintf(stderr, "printing dot file failed\n");
    }

    free(nodes);
    agclose(graph);
}

#define NAMEBUFSIZE 16

void nfa_state_to_graph(Agraph_t *graph, Agnode_t **nodes, struct nfa_state *to, Agnode_t *from, char *edge_label) {
    if (!nodes[to->id]) {
        static char namebuf[NAMEBUFSIZE] = "";
        char label[2] = "";

        // create a node
        sprintf(namebuf, "n%d", to->id);
        Agnode_t *node = nodes[to->id] = agnode(graph, namebuf, 1);

        // set the node label
        sprintf(namebuf, "%d", to->id);
        agset(node, "label", namebuf);

        switch (to->type) {
            case RX_ACCEPTING_STATE:
                agset(node, "color", "red");
                agset(node, "fontcolor", "red");
                break;
            case RX_EPSILON_STATE:
                nfa_state_to_graph(graph, nodes, to->next, node, "ε");
                break;
            case RX_BRANCH_STATE:
                agset(node, "color", "darkgreen");
                agset(node, "fontcolor", "darkgreen");
                nfa_state_to_graph(graph, nodes, to->left, node, "ε");
                nfa_state_to_graph(graph, nodes, to->right, node, "ε");
                break;
            case RX_DOTALL_STATE:
                agset(node, "color", "pink");
                agset(node, "fontcolor", "pink");
                label[0] = '.';
                nfa_state_to_graph(graph, nodes, to->next, node, label);
                break;
            case RX_CLASS_STATE:
                agset(node, "color", "purple");
                agset(node, "fontcolor", "purple");
                nfa_state_to_graph(graph, nodes, to->next, node, "[a-z]");
                break;
            case RX_CHAR_STATE:
                agset(node, "color", "blue");
                agset(node, "fontcolor", "blue");
                label[0] = to->ch;
                nfa_state_to_graph(graph, nodes, to->next, node, label);
                break;
        }
    }

    // make an arrow
    if (from) {
        Agedge_t *edge = agedge(graph, from, nodes[to->id], NULL, 1);
        agset(edge, "label", edge_label);
    }
}
