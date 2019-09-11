#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <base/random.h>
#include "ast.h"
#include "gensource.h"

/*

min-height  2
max-height  9
9   program
8       block
7           if_stmt
6               rel_expr
5                   lt_rel
4                       add_rel
3                           factor_term
2                               id_factor
4                       add_rel
3                           factor_term
2                               num_factor
6               block_stmt
5                   block
4                       expr_stmt
3                           assign_expr
2                               add_rel
1                                   factor_term
0                                       id_factor
2                               add_rel
1                                   factor_term
0                                       num_factor
3           while_stmt

min
2        program
1        block

2        *stmt*
6        expr_stmt
6        if_stmt
6        while_stmt
6        do_stmt
2        block_stmt

(5)        *expr*
6        assign_expr
5        rel_expr

(4)        *rel*
5        lt_rel
5        lteq_rel
4        add_rel

(3)        *add*
4        plus_add
3        term_add

(2)      *term*
3        mult_term
2        factor_term

(1)      *factor*
5        subexpr_factor
1        num_factor
1        id_factor

Hrm, not even close...
We can't write this to generate based on "node" depth, but rather
on the minimum valid syntax tree
program - block
program - block - stmt - expr - rel - add - term - factor
*/

static bool falling(int depth) {
    return depth >= 0;
}

static int decr(int depth) {
    return depth - 1;
}

static int subd(int depth) {
    return depth - depth ? randr(1, depth) : 1;
}


struct program *gen_program(struct gendims dims) {
    return init_program(gen_block(dims, randr(dims.mind, dims.maxd)));
}

struct block *gen_block(struct gendims dims, int depth) {
    struct block *block = NULL;

    if (falling(depth)) {
        block = init_block();

        struct stmt * stmt = NULL;
        for (int i = 0; i < randr(dims.minw, dims.maxw); i++) {
            if ((stmt = gen_stmt(dims, subd(depth)))) {
                append_stmt(block, stmt);
            }
        }
    }

    return block;
}

struct stmt *gen_stmt(struct gendims dims, int depth) {
    struct stmt *stmt = NULL;

    if (falling(depth)) {
        enum stmt_type stmt_type = randr(EXPR, BLOCK);
        void *stmt_val = NULL;

        switch (stmt_type) {
            case EXPR:
                stmt_val = gen_expr_stmt(depth);
                break;
            case IF:
                stmt_val = gen_if_stmt(dims, depth);
                break;
            case WHILE:
                stmt_val = gen_while_stmt(dims, depth);
                break;
            case DO:
                stmt_val = gen_do_stmt(dims, depth);
                break;
            case BLOCK:
                stmt_val = gen_block_stmt(dims, depth);
                break;
        }

        if (stmt_val) {
            stmt = init_stmt(stmt_type, stmt_val);
        }
    }

    return stmt;
}

struct expr_stmt *gen_expr_stmt(int depth) {
    struct expr_stmt *expr_stmt = NULL;

    if (falling(depth)) {
        struct expr *expr = NULL;

        if ((expr = gen_expr(decr(depth)))) {
            expr_stmt = init_expr_stmt(expr);
        }
    }

    return expr_stmt;
}

struct if_stmt *gen_if_stmt(struct gendims dims, int depth) {
    struct if_stmt *if_stmt = NULL;

    if (falling(depth)) {
        struct expr *expr = NULL;
        struct stmt *stmt = NULL;

        if ((expr = gen_expr(decr(depth))) && (stmt = gen_stmt(dims, decr(depth)))) {
            if_stmt = init_if_stmt(expr, stmt);
        } else if (expr) free_expr(expr);
    }

    return if_stmt;
}

struct while_stmt *gen_while_stmt(struct gendims dims, int depth) {
    struct while_stmt *while_stmt = NULL;

    if (falling(depth)) {
        struct expr *expr = NULL;
        struct stmt *stmt = NULL;

        if ((expr = gen_expr(decr(depth))) && (stmt = gen_stmt(dims, decr(depth)))) {
            while_stmt = init_while_stmt(expr, stmt);
        } else if (expr) free_expr(expr);
    }

    return while_stmt;
}

struct do_stmt *gen_do_stmt(struct gendims dims, int depth) {
    struct do_stmt *do_stmt = NULL;

    if (falling(depth)) {
        struct stmt *stmt = NULL;
        struct expr *expr = NULL;

        if ((stmt = gen_stmt(dims, decr(depth))) && (expr = gen_expr(decr(depth)))) {
            do_stmt = init_do_stmt(stmt, expr);
        } else if (stmt) free_stmt(stmt);
    }

    return do_stmt;
}

struct block_stmt *gen_block_stmt(struct gendims dims, int depth) {
    struct block_stmt *block_stmt = NULL;

    if (falling(depth)) {
        struct block *block = NULL;

        if ((block = gen_block(dims, decr(depth)))) {
            block_stmt = init_block_stmt(block);
        }
    }

    return block_stmt;
}

struct expr *gen_expr(int depth) {
    struct expr *expr = NULL;

    if (falling(depth)) {
        enum expr_type expr_type = randr(ASSIGN, REL);
        void *expr_val = NULL;

        switch (expr_type) {
            case ASSIGN:
                expr_val = gen_assign_expr(depth);
                break;
            case REL:
                expr_val = gen_rel_expr(depth);
                break;
        }

        if (expr_val) {
            expr = init_expr(expr_type, expr_val);
        }
    }

    return expr;
}

struct assign_expr *gen_assign_expr(int depth) {
    struct assign_expr *assign_expr = NULL;

    if (falling(depth)) {
        struct rel *rel = NULL;
        struct expr *expr = NULL;

        if ((rel = gen_rel(decr(depth))) && (expr = gen_expr(decr(depth)))) {
            assign_expr = init_assign_expr(rel, expr);
        } else if (rel) free_rel(rel);
    }

    return assign_expr;
}

struct rel_expr *gen_rel_expr(int depth) {
    struct rel_expr *rel_expr = NULL;

    if (falling(depth)) {
        struct rel *rel = NULL;

        if ((rel = gen_rel(decr(depth)))) {
            rel_expr = init_rel_expr(rel);
        }
    }

    return rel_expr;
}

struct rel *gen_rel(int depth) {
    struct rel *rel = NULL;

    if (falling(depth)) {
        enum rel_type rel_type = randr(LT, ADD);
        void *rel_val = NULL;

        switch (rel_type) {
            case LT:
                rel_val = gen_lt_rel(depth);
                break;
            case LT_EQ:
                rel_val = gen_lteq_rel(depth);
                break;
            case ADD:
                rel_val = gen_rel_expr(depth);
                break;
        }

        if (rel_val) {
            rel = init_rel(rel_type, rel_val);
        }
    }

    return rel;
}

struct lt_rel *gen_lt_rel(int depth) {
    struct lt_rel *lt_rel = NULL;

    if (falling(depth)) {
        struct rel *rel = NULL;
        struct add *add = NULL;

        if ((rel = gen_rel(decr(depth))) && (add = gen_add(decr(depth)))) {
            lt_rel = init_lt_rel(rel, add);
        } else if (rel) free_rel(rel);
    }

    return lt_rel;
}

struct lteq_rel *gen_lteq_rel(int depth) {
    struct lteq_rel *lteq_rel = NULL;

    if (falling(depth)) {
        struct rel *rel = NULL;
        struct add *add = NULL;

        if ((rel = gen_rel(decr(depth))) && (add = gen_add(decr(depth)))) {
            lteq_rel = init_lteq_rel(rel, add);
        } else if (rel) free_rel(rel);
    }

    return lteq_rel;
}

struct add_rel *gen_add_rel(int depth) {
    struct add_rel *add_rel = NULL;

    if (falling(depth)) {
        struct add *add = NULL;

        if ((add = gen_add(decr(depth)))) {
            add_rel = init_add_rel(add);
        }
    }

    return add_rel;
}

struct add *gen_add(int depth) {
    struct add *add = NULL;

    if (falling(depth)) {
        enum add_type add_type = randr(PLUS, TERM);
        void *add_val = NULL;

        switch (add_type) {
            case PLUS:
                add_val = gen_plus_add(depth);
                break;
            case TERM:
                add_val = gen_term_add(depth);
                break;
        }

        if (add_val) {
            add = init_add(add_type, add_val);
        }
    }

    return add;
}

struct plus_add *gen_plus_add(int depth) {
    struct plus_add *plus_add = NULL;

    if (falling(depth)) {
        struct add *add = NULL;
        struct term *term = NULL;

        if ((add = gen_add(decr(depth))) && (term = gen_term(decr(depth)))) {
            plus_add = init_plus_add(add, term);
        } else if (add) free_add(add);
    }

    return plus_add;
}

struct term_add *gen_term_add(int depth) {
    struct term_add *term_add = NULL;

    if (falling(depth)) {
        struct term *term = NULL;

        if ((term = gen_term(decr(depth)))) {
            term_add = init_term_add(term);
        }
    }

    return term_add;
}

struct term *gen_term(int depth) {
    struct term *term = NULL;

    if (falling(depth)) {
        enum term_type term_type = randr(MULT, FACTOR);
        void *term_val = NULL;

        switch (term_type) {
            case MULT:
                term_val = gen_mult_term(depth);
                break;
            case FACTOR:
                term_val = gen_factor_term(depth);
                break;
        }

        if (term_val) {
            term = init_term(term_type, term_val);
        }
    }

    return term;
}

struct mult_term *gen_mult_term(int depth) {
    struct mult_term *mult_term = NULL;

    if (falling(depth)) {
        struct term *term = NULL;
        struct factor *factor = NULL;

        if ((term = gen_term(decr(depth))) && (factor = gen_factor(decr(depth)))) {
            mult_term = init_mult_term(term, factor);
        } else if (term) free_term(term);
    }

    return mult_term;
}

struct factor_term *gen_factor_term(int depth) {
    struct factor_term *factor_term = NULL;

    if (falling(depth)) {
        struct factor *factor = NULL;

        if ((factor = gen_factor(decr(depth)))) {
            factor_term = init_factor_term(factor);
        }
    }

    return factor_term;
}

struct factor *gen_factor(int depth) {
    struct factor *factor = NULL;

    if (falling(depth)) {
        enum factor_type factor_type = randr(SUBEXPR, ID);
        void *factor_val = NULL;

        switch (factor_type) {
            case SUBEXPR:
                factor_val = gen_subexpr_factor(depth);
                break;
            case NUM:
                factor_val = gen_num_factor(depth);
                break;
            case ID:
                factor_val = gen_id_factor(depth);
                break;
        }

        if (factor_val) {
            factor = init_factor(factor_type, factor_val);
        }
    }

    return factor;
}

struct subexpr_factor *gen_subexpr_factor(int depth) {
    struct subexpr_factor *subexpr_factor = NULL;

    if (falling(depth)) {
        struct expr *expr = NULL;

        if ((expr = gen_expr(decr(depth)))) {
            subexpr_factor = init_subexpr_factor(expr);
        }
    }

    return subexpr_factor;
}

struct num_factor *gen_num_factor(int depth) {
    struct num_factor *num_factor = NULL;

    if (falling(depth)) {
        char *num = NULL;
        if (asprintf(&num, "%d", randr(0, 65535)) != -1) {
            num_factor = init_num_factor(num);
        }
    }

    return num_factor;
}

struct id_factor *gen_id_factor(int depth) {
    struct id_factor *id_factor = NULL;

    if (falling(depth)) {
        char *id = NULL;
        if (asprintf(&id, "var%d", randr(0, 65535)) != -1) {
            id_factor = init_id_factor(id);
        }
    }

    return id_factor;
}

