#include <linked_list.h>
#include <assert.h>
#include "ast.h"

#define FREEFN (void (*) (void *))

struct program *init_program(struct block *block) {
    struct program *program = malloc(sizeof *program);
    assert(program != NULL);
    program->block = block;
    return program;
}

struct block *init_block() {
    struct block *block = malloc(sizeof *block);
    assert(block != NULL);
    block->stmts = init_list();
    return block;
}

struct block *append_stmt(struct block *block, struct stmt *stmt) {
    append(block->stmts, stmt);
    return block;
}

struct stmt *init_stmt(enum stmt_type type, void *val) {
    struct stmt *stmt = malloc(sizeof *stmt);
    assert(stmt != NULL);

    switch (type) {
        case EXPR:
            stmt->expr_stmt = val;
            break;
        case IF:
            stmt->if_stmt = val;
            break;
        case WHILE:
            stmt->while_stmt = val;
            break;
        case DO:
            stmt->do_stmt = val;
            break;
        case BLOCK:
            stmt->block_stmt = val;
            break;
        default:
            break;
    };

    stmt->type = type;
    return stmt;
}

struct expr_stmt *init_expr_stmt(struct expr *expr) {
    struct expr_stmt *expr_stmt = malloc(sizeof *expr_stmt);
    assert(expr_stmt != NULL);
    expr_stmt->expr = expr;
    return expr_stmt;
}

struct if_stmt *init_if_stmt(struct expr *expr, struct stmt *stmt) {
    struct if_stmt *if_stmt = malloc(sizeof *if_stmt);
    assert(if_stmt != NULL);
    if_stmt->expr = expr;
    if_stmt->stmt = stmt;
    return if_stmt;
}

struct while_stmt *init_while_stmt(struct expr *expr, struct stmt *stmt) {
    struct while_stmt *while_stmt = malloc(sizeof *while_stmt);
    assert(while_stmt != NULL);
    while_stmt->expr = expr;
    while_stmt->stmt = stmt;
    return while_stmt;
}

struct do_stmt *init_do_stmt(struct stmt *stmt, struct expr *expr) {
    struct do_stmt *do_stmt = malloc(sizeof *do_stmt);
    assert(do_stmt != NULL);
    do_stmt->stmt = stmt;
    do_stmt->expr = expr;
    return do_stmt;
}

struct block_stmt *init_block_stmt(struct block *block) {
    struct block_stmt *block_stmt = malloc(sizeof *block_stmt);
    assert(block_stmt != NULL);
    block_stmt->block = block;
    return block_stmt;
}

struct expr *init_expr(enum expr_type type, void *val) {
    struct expr *expr = malloc(sizeof *expr);
    assert(expr != NULL);

    switch (type) {
        case ASSIGN:
            expr->assign_expr = val;
            break;
        case REL:
            expr->rel_expr = val;
            break;
        default:
            break;
    };

    expr->type = type;
    return expr;
}

struct assign_expr *init_assign_expr(struct rel *rel, struct expr *expr) {
    struct assign_expr *assign_expr = malloc(sizeof *assign_expr);
    assert(assign_expr != NULL);
    assign_expr->rel = rel;
    assign_expr->expr = expr;
    return assign_expr;
}

struct rel_expr *init_rel_expr(struct rel *rel) {
    struct rel_expr *rel_expr = malloc(sizeof *rel_expr);
    assert(rel_expr != NULL);
    rel_expr->rel = rel;
    return rel_expr;
}

struct rel *init_rel(enum rel_type type, void *val) {
    struct rel *rel = malloc(sizeof *rel);
    assert(rel != NULL);

    switch (type) {
        case LT:
            rel->lt_rel = val;
            break;
        case LT_EQ:
            rel->lteq_rel = val;
            break;
        case ADD:
            rel->add_rel = val;
            break;
        default:
            break;
    };

    rel->type = type;
    return rel;
}

struct lt_rel *init_lt_rel(struct rel *rel, struct add *add) {
    struct lt_rel *lt_rel = malloc(sizeof *lt_rel);
    assert(lt_rel != NULL);
    lt_rel->rel = rel;
    lt_rel->add = add;
    return lt_rel;
}

struct lteq_rel *init_lteq_rel(struct rel *rel, struct add *add) {
    struct lteq_rel *lteq_rel = malloc(sizeof *lteq_rel);
    assert(lteq_rel != NULL);
    lteq_rel->rel = rel;
    lteq_rel->add = add;
    return lteq_rel;
}

struct add_rel *init_add_rel(struct add *add) {
    struct add_rel *add_rel = malloc(sizeof *add_rel);
    assert(add_rel != NULL);
    add_rel->add = add;
    return add_rel;
}

struct add *init_add(enum add_type type, void *val) {
    struct add *add = malloc(sizeof *add);
    assert(add != NULL);

    switch (type) {
        case PLUS:
            add->plus_add = val;
            break;
        case TERM:
            add->term_add = val;
            break;
        default:
            break;
    };

    add->type = type;
    return add;
}

struct plus_add *init_plus_add(struct add *add, struct term *term) {
    struct plus_add *plus_add = malloc(sizeof *plus_add);
    assert(plus_add != NULL);
    plus_add->add = add;
    plus_add->term = term;
    return plus_add;
}

struct term_add *init_term_add(struct term *term) {
    struct term_add *term_add = malloc(sizeof *term_add);
    assert(term_add != NULL);
    term_add->term = term;
    return term_add;
}

struct term *init_term(enum term_type type, void *val) {
    struct term *term = malloc(sizeof *term);
    assert(term != NULL);

    switch (type) {
        case MULT:
            term->mult_term = val;
            break;
        case FACTOR:
            term->factor_term = val;
            break;
        default:
            break;
    };

    term->type = type;
    return term;
}

struct mult_term *init_mult_term(struct term *term, struct factor *factor) {
    struct mult_term *mult_term = malloc(sizeof *mult_term);
    assert(mult_term != NULL);
    mult_term->term = term;
    mult_term->factor = factor;
    return mult_term;
}

struct factor_term *init_factor_term(struct factor *factor) {
    struct factor_term *factor_term = malloc(sizeof *factor_term);
    assert(factor_term != NULL);
    factor_term->factor = factor;
    return factor_term;
}

struct factor *init_factor(enum factor_type type, void *val) {
    struct factor *factor = malloc(sizeof *factor);
    assert(factor != NULL);

    switch (type) {
        case SUBEXPR:
            factor->subexpr_factor = val;
            break;
        case NUM:
            factor->num_factor = val;
            break;
        case ID:
            factor->id_factor = val;
            break;
        default:
            break;
    };

    factor->type = type;
    return factor;
}

struct subexpr_factor *init_subexpr_factor(struct expr *expr) {
    struct subexpr_factor *subexpr_factor = malloc(sizeof *subexpr_factor);
    assert(subexpr_factor != NULL);
    subexpr_factor->expr = expr;
    return subexpr_factor;
}

struct num_factor *init_num_factor(long num) {
    struct num_factor *num_factor = malloc(sizeof *num_factor);
    assert(num_factor != NULL);
    num_factor->num = num;
    return num_factor;
}

struct id_factor *init_id_factor(char *id) {
    struct id_factor *id_factor = malloc(sizeof *id_factor);
    assert(id_factor != NULL);
    id_factor->id = id;
    return id_factor;
}

void free_program(struct program *program) {
    free_block(program->block);
    free(program);
}

void free_block(struct block *block) {
    free_list(block->stmts, FREEFN free_stmt);
    block->stmts = NULL;
    free(block);
}

void free_stmt(struct stmt *stmt) {
    switch (stmt->type) {
        case EXPR:
            free_expr_stmt(stmt->expr_stmt);
            stmt->expr_stmt = NULL;
            break;
        case IF:
            free_if_stmt(stmt->if_stmt);
            stmt->if_stmt = NULL;
            break;
        case WHILE:
            free_while_stmt(stmt->while_stmt);
            stmt->while_stmt = NULL;
            break;
        case DO:
            free_do_stmt(stmt->do_stmt);
            stmt->do_stmt = NULL;
            break;
        case BLOCK:
            free_block_stmt(stmt->block_stmt);
            stmt->block_stmt = NULL;
            break;
        default:
            break;
    };

    free(stmt);
}

void free_expr_stmt(struct expr_stmt *stmt) {
    free_expr(stmt->expr);
    free(stmt);
}

void free_if_stmt(struct if_stmt *stmt) {
    free_expr(stmt->expr);
    free_stmt(stmt->stmt);
    free(stmt);
}

void free_while_stmt(struct while_stmt *stmt) {
    free_expr(stmt->expr);
    free_stmt(stmt->stmt);
    free(stmt);
}

void free_do_stmt(struct do_stmt *stmt) {
    free_stmt(stmt->stmt);
    free_expr(stmt->expr);
    free(stmt);
}

void free_block_stmt(struct block_stmt *stmt) {
    free_block(stmt->block);
    free(stmt);
}

void free_expr(struct expr *expr) {
    switch (expr->type) {
        case ASSIGN:
            free_assign_expr(expr->assign_expr);
            expr->assign_expr = NULL;
            break;
        case REL:
            free_rel_expr(expr->rel_expr);
            expr->rel_expr = NULL;
            break;
        default:
            break;
    };

    free(expr);
}

void free_assign_expr(struct assign_expr *expr) {
    free_rel(expr->rel);
    free_expr(expr->expr);
    free(expr);
}

void free_rel_expr(struct rel_expr *expr) {
    free_rel(expr->rel);
    free(expr);
}

void free_rel(struct rel *rel) {
    switch (rel->type) {
        case LT:
            free_lt_rel(rel->lt_rel);
            rel->lt_rel = NULL;
            break;
        case LT_EQ:
            free_lteq_rel(rel->lteq_rel);
            rel->lteq_rel = NULL;
            break;
        case ADD:
            free_add_rel(rel->add_rel);
            rel->add_rel = NULL;
            break;
        default:
            break;
    };

    free(rel);
}

void free_lt_rel(struct lt_rel *rel) {
    free_rel(rel->rel);
    free_add(rel->add);
    free(rel);
}

void free_lteq_rel(struct lteq_rel *rel) {
    free_rel(rel->rel);
    free_add(rel->add);
    free(rel);
}

void free_add_rel(struct add_rel *rel) {
    free_add(rel->add);
    free(rel);
}

void free_add(struct add *add) {
    switch (add->type) {
        case PLUS:
            free_plus_add(add->plus_add);
            add->plus_add = NULL;
            break;
        case TERM:
            free_term_add(add->term_add);
            add->term_add = NULL;
            break;
        default:
            break;
    };

    free(add);
}

void free_plus_add(struct plus_add *add) {
    free_add(add->add);
    free_term(add->term);
    free(add);
}

void free_term_add(struct term_add *add) {
    free_term(add->term);
    free(add);
}

void free_term(struct term *term) {
    switch (term->type) {
        case MULT:
            free_mult_term(term->mult_term);
            term->mult_term = NULL;
            break;
        case FACTOR:
            free_factor_term(term->factor_term);
            term->factor_term = NULL;
            break;
        default:
            break;
    };

    free(term);
}

void free_mult_term(struct mult_term *term) {
    free_term(term->term);
    free_factor(term->factor);
    free(term);
}

void free_factor_term(struct factor_term *term) {
    free_factor(term->factor);
    free(term);
}

void free_factor(struct factor *factor) {
    switch (factor->type) {
        case SUBEXPR:
            free_subexpr_factor(factor->subexpr_factor);
            factor->subexpr_factor = NULL;
            break;
        case NUM:
            free_num_factor(factor->num_factor);
            factor->num_factor = NULL;
            break;
        case ID:
            free_id_factor(factor->id_factor);
            factor->id_factor = NULL;
            break;
        default:
            break;
    };

    free(factor);
}

void free_subexpr_factor(struct subexpr_factor *factor) {
    free_expr(factor->expr);
    free(factor);
}

void free_num_factor(struct num_factor *factor) {
    free(factor);
}

void free_id_factor(struct id_factor *factor) {
    free(factor->id);
    free(factor);
}

