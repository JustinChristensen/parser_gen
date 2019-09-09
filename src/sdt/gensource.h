#ifndef SDT_GENSOURCE_H_
#define SDT_GENSOURCE_H_ 1

#include <stdlib.h>
#include "ast.h"

struct gendims {
    int minw;
    int maxw;
    int mind;
    int maxd;
};

struct program *gen_program(struct gendims dims);
struct block *gen_block(struct gendims dims, int depth);
struct stmt *gen_stmt(struct gendims dims, int depth);
struct expr_stmt *gen_expr_stmt(int depth);
struct if_stmt *gen_if_stmt(struct gendims dims, int depth);
struct while_stmt *gen_while_stmt(struct gendims dims, int depth);
struct do_stmt *gen_do_stmt(struct gendims dims, int depth);
struct block_stmt *gen_block_stmt(struct gendims dims, int depth);
struct expr *gen_expr(int depth);
struct assign_expr *gen_assign_expr(int depth);
struct rel_expr *gen_rel_expr(int depth);
struct rel *gen_rel(int depth);
struct lt_rel *gen_lt_rel(int depth);
struct lteq_rel *gen_lteq_rel(int depth);
struct add_rel *gen_add_rel(int depth);
struct add *gen_add(int depth);
struct plus_add *gen_plus_add(int depth);
struct term_add *gen_term_add(int depth);
struct term *gen_term(int depth);
struct mult_term *gen_mult_term(int depth);
struct factor_term *gen_factor_term(int depth);
struct factor *gen_factor(int depth);
struct subexpr_factor *gen_subexpr_factor(int depth);
struct num_factor *gen_num_factor(int depth);
struct id_factor *gen_id_factor(int depth);

#endif // SDT_GENSOURCE_H_
