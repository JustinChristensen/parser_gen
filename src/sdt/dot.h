#ifndef SDT_DOT_H_
#define SDT_DOT_H_ 1

#include <stdio.h>
#include <cgraph.h>
#include "ast.h"

#define TOGRAPHFN (void (*) (Agraph_t *, Agnode_t *, void *))

void print_dot(FILE *handle, void *ast, char *input, void (*to_graph) (Agraph_t *graph, Agnode_t *parent, void *ast));
void program_to_graph(Agraph_t *graph, Agnode_t *parent, struct program *program);
void block_to_graph(Agraph_t *graph, Agnode_t *parent, struct block *block);
void stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct stmt *stmt);
void expr_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr_stmt *stmt);
void if_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct if_stmt *stmt);
void while_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct while_stmt *stmt);
void do_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct do_stmt *stmt);
void block_stmt_to_graph(Agraph_t *graph, Agnode_t *parent, struct block_stmt *stmt);
void expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct expr *expr);
void assign_expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct assign_expr *expr);
void rel_expr_to_graph(Agraph_t *graph, Agnode_t *parent, struct rel_expr *expr);
void rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct rel *rel);
void lt_rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct lt_rel *rel);
void lteq_rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct lteq_rel *rel);
void add_rel_to_graph(Agraph_t *graph, Agnode_t *parent, struct add_rel *rel);
void add_to_graph(Agraph_t *graph, Agnode_t *parent, struct add *add);
void plus_add_to_graph(Agraph_t *graph, Agnode_t *parent, struct plus_add *add);
void term_add_to_graph(Agraph_t *graph, Agnode_t *parent, struct term_add *add);
void term_to_graph(Agraph_t *graph, Agnode_t *parent, struct term *term);
void mult_term_to_graph(Agraph_t *graph, Agnode_t *parent, struct mult_term *term);
void factor_term_to_graph(Agraph_t *graph, Agnode_t *parent, struct factor_term *term);
void factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct factor *factor);
void subexpr_factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct subexpr_factor *factor);
void num_factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct num_factor *factor);
void id_factor_to_graph(Agraph_t *graph, Agnode_t *parent, struct id_factor *factor);

#endif // SDT_DOT
