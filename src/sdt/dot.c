#include <cgraph.h>
#include <stdio.h>
#include <base/list.h>
#include <base/graphviz.h>
#include "dot.h"

void program_to_graph(Agraph_t *graph, Agnode_t *parent, struct program *program) {
    block_to_graph(graph, append_node(graph, NULL, "program"), program->block);
}

void block_to_graph(Agraph_t *graph, Agnode_t *parent, struct block *block) {
    Agnode_t *block_node = append_node(graph, parent, "block");

    for (struct node *node = head(block->stmts); node; node = next(node)) {
        stmt_to_graph(graph, block_node, value(node));
    }
}

void stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct stmt *stmt) {
    switch (stmt->type) {
        case EXPR:
            expr_stmt_to_graph(graph, parent, stmt->expr_stmt);
            break;
        case IF:
            if_stmt_to_graph(graph, parent, stmt->if_stmt);
            break;
        case WHILE:
            while_stmt_to_graph(graph, parent, stmt->while_stmt);
            break;
        case DO:
            do_stmt_to_graph(graph, parent, stmt->do_stmt);
            break;
        case BLOCK:
            block_stmt_to_graph(graph, parent, stmt->block_stmt);
            break;
    };
}

void expr_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr_stmt *stmt) {
    Agnode_t *stmt_node = append_node(graph, parent, "expr_stmt");
    expr_to_graph(graph, stmt_node, stmt->expr);
}

void if_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct if_stmt *stmt) {
    Agnode_t *stmt_node = append_node(graph, parent, "if_stmt");
    expr_to_graph(graph, stmt_node, stmt->expr);
    stmt_to_graph(graph, stmt_node, stmt->stmt);
}

void while_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct while_stmt *stmt) {
    Agnode_t *stmt_node = append_node(graph, parent, "while_stmt");
    expr_to_graph(graph, stmt_node, stmt->expr);
    stmt_to_graph(graph, stmt_node, stmt->stmt);
}

void do_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct do_stmt *stmt) {
    Agnode_t *stmt_node = append_node(graph, parent, "do_stmt");
    stmt_to_graph(graph, stmt_node, stmt->stmt);
    expr_to_graph(graph, stmt_node, stmt->expr);
}

void block_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct block_stmt *stmt) {
    Agnode_t *stmt_node = append_node(graph, parent, "block_stmt");
    block_to_graph(graph, stmt_node, stmt->block);
}

void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr) {
    switch (expr->type) {
        case ASSIGN:
            assign_expr_to_graph(graph, parent, expr->assign_expr);
            break;
        case REL:
            rel_expr_to_graph(graph, parent, expr->rel_expr);
            break;
    };
}

void assign_expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct assign_expr *expr) {
    Agnode_t *expr_node = append_node(graph, parent, "=");
    rel_to_graph(graph, expr_node, expr->rel);
    expr_to_graph(graph, expr_node, expr->expr);
}

void rel_expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct rel_expr *expr) {
    Agnode_t *expr_node = append_node(graph, parent, "rel");
    rel_to_graph(graph, expr_node, expr->rel);
}

void rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct rel *rel) {
    switch (rel->type) {
        case LT:
            lt_rel_to_graph(graph, parent, rel->lt_rel);
            break;
        case LT_EQ:
            lteq_rel_to_graph(graph, parent, rel->lteq_rel);
            break;
        case ADD:
            add_rel_to_graph(graph, parent, rel->add_rel);
            break;
    };
}

void lt_rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct lt_rel *rel) {
    Agnode_t *rel_node = append_node(graph, parent, "<");
    rel_to_graph(graph, rel_node, rel->rel);
    add_to_graph(graph, rel_node, rel->add);
}

void lteq_rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct lteq_rel *rel) {
    Agnode_t *rel_node = append_node(graph, parent, "<=");
    rel_to_graph(graph, rel_node, rel->rel);
    add_to_graph(graph, rel_node, rel->add);
}

void add_rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct add_rel *rel) {
    Agnode_t *rel_node = append_node(graph, parent, "add");
    add_to_graph(graph, rel_node, rel->add);
}

void add_to_graph(Agraph_t *graph, Agnode_t *parent, struct add *add) {
    switch (add->type) {
        case PLUS:
            plus_add_to_graph(graph, parent, add->plus_add);
            break;
        case TERM:
            term_add_to_graph(graph, parent, add->term_add);
            break;
    };
}

void plus_add_to_graph(Agraph_t *graph, Agnode_t *parent, struct plus_add *add) {
    Agnode_t *add_node = append_node(graph, parent, "+");
    add_to_graph(graph, add_node, add->add);
    term_to_graph(graph, add_node, add->term);
}

void term_add_to_graph(Agraph_t *graph, Agnode_t *parent, struct term_add *add) {
    Agnode_t *add_node_ = append_node(graph, parent, "term");
    term_to_graph(graph, add_node_, add->term);
}

void term_to_graph(Agraph_t *graph, Agnode_t *parent, struct term *term) {
    switch (term->type) {
        case MULT:
            mult_term_to_graph(graph, parent, term->mult_term);
            break;
        case FACTOR:
            factor_term_to_graph(graph, parent, term->factor_term);
            break;
    };
}

void mult_term_to_graph(Agraph_t *graph, Agnode_t *parent, struct mult_term *term) {
    Agnode_t *term_node = append_node(graph, parent, "*");
    term_to_graph(graph, term_node, term->term);
    factor_to_graph(graph, term_node, term->factor);
}

void factor_term_to_graph(Agraph_t *graph, Agnode_t *parent, struct factor_term *term) {
    Agnode_t *term_node = append_node(graph, parent, "factor");
    factor_to_graph(graph, term_node, term->factor);
}

void factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct factor *factor) {
    switch (factor->type) {
        case SUBEXPR:
            subexpr_factor_to_graph(graph, parent, factor->subexpr_factor);
            break;
        case NUM:
            num_factor_to_graph(graph, parent, factor->num_factor);
            break;
        case ID:
            id_factor_to_graph(graph, parent, factor->id_factor);
            break;
    };
}

void subexpr_factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct subexpr_factor *factor) {
    Agnode_t *factor_node = append_node(graph, parent, "(expr)");
    expr_to_graph(graph, factor_node, factor->expr);
}

void num_factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct num_factor *factor) {
    append_node(graph, parent, factor->num);
}

void id_factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct id_factor *factor) {
    append_node(graph, parent, factor->id);
}
