#include <stdlib.h>
#include <stdio.h>
#include <base/list.h>
#include <base/string.h>
#include "ast.h"
#include "source.h"

#define BUFFER_SIZE 1000000
void print_source(FILE *handle, void *ast, char *(*to_source) (char *srcbuf, int indent_level, void *ast)) {
    char srcbuf[BUFFER_SIZE] = "";
    (*to_source)(srcbuf, 0, ast);
    fprintf(handle, "%s", srcbuf);
}

#define INDENT ' '
#define INDENT_NUM 4
static char *_indent(char *srcbuf, int indent_level) {
    return repeat(srcbuf, INDENT, indent_level * INDENT_NUM);
}

char *program_to_source(char *srcbuf, int indent_level, struct program *program) {
    return block_to_source(srcbuf, indent_level, program->block);
}

char *block_to_source(char *srcbuf, int indent_level, struct block *block) {
    srcbuf = putln(srcbuf, "{");
    indent_level++;
    for (struct node *node = head(block->stmts); node; node = next(node)) {
        srcbuf = _indent(srcbuf, indent_level);
        srcbuf = stmt_to_source(srcbuf, indent_level, value(node));
    }
    indent_level--;
    srcbuf = _indent(srcbuf, indent_level);
    srcbuf = putln(srcbuf, "}");
    return srcbuf;
}

char *stmt_to_source(char *srcbuf, int indent_level, struct stmt *stmt) {
    switch (stmt->type) {
        case EXPR:
            srcbuf = expr_stmt_to_source(srcbuf, indent_level, stmt->expr_stmt);
            break;
        case IF:
            srcbuf = if_stmt_to_source(srcbuf, indent_level, stmt->if_stmt);
            break;
        case WHILE:
            srcbuf = while_stmt_to_source(srcbuf, indent_level, stmt->while_stmt);
            break;
        case DO:
            srcbuf = do_stmt_to_source(srcbuf, indent_level, stmt->do_stmt);
            break;
        case BLOCK:
            srcbuf = block_stmt_to_source(srcbuf, indent_level, stmt->block_stmt);
            break;
    };

    return srcbuf;
}

char *expr_stmt_to_source(char *srcbuf, int indent_level, struct expr_stmt *stmt) {
    srcbuf = expr_to_source(srcbuf, indent_level, stmt->expr);
    srcbuf = putln(srcbuf, ";");
    return srcbuf;
}

char *if_stmt_to_source(char *srcbuf, int indent_level, struct if_stmt *stmt) {
    srcbuf = put(srcbuf, "if (");
    srcbuf = expr_to_source(srcbuf, indent_level, stmt->expr);
    srcbuf = put(srcbuf, ") ");
    srcbuf = stmt_to_source(srcbuf, indent_level, stmt->stmt);
    return srcbuf;
}

char *while_stmt_to_source(char *srcbuf, int indent_level, struct while_stmt *stmt) {
    srcbuf = put(srcbuf, "while (");
    srcbuf = expr_to_source(srcbuf, indent_level, stmt->expr);
    srcbuf = put(srcbuf, ") ");
    srcbuf = stmt_to_source(srcbuf, indent_level, stmt->stmt);
    return srcbuf;
}

char *do_stmt_to_source(char *srcbuf, int indent_level, struct do_stmt *stmt) {
    srcbuf = put(srcbuf, "do ");
    srcbuf = stmt_to_source(srcbuf, indent_level, stmt->stmt);
    srcbuf = _indent(srcbuf, indent_level);
    srcbuf = put(srcbuf, "while (");
    srcbuf = expr_to_source(srcbuf, indent_level, stmt->expr);
    srcbuf = putln(srcbuf, ");");
    return srcbuf;
}

char *block_stmt_to_source(char *srcbuf, int indent_level, struct block_stmt *stmt) {
    return block_to_source(srcbuf, indent_level, stmt->block);
}

char *expr_to_source(char *srcbuf, int indent_level, struct expr *expr) {
    switch (expr->type) {
        case ASSIGN:
            srcbuf = assign_expr_to_source(srcbuf, indent_level, expr->assign_expr);
            break;
        case REL:
            srcbuf = rel_expr_to_source(srcbuf, indent_level, expr->rel_expr);
            break;
    };

    return srcbuf;
}

char *assign_expr_to_source(char *srcbuf, int indent_level, struct assign_expr *expr) {
    srcbuf = rel_to_source(srcbuf, indent_level, expr->rel);
    srcbuf = put(srcbuf, " = ");
    srcbuf = expr_to_source(srcbuf, indent_level, expr->expr);
    return srcbuf;
}

char *rel_expr_to_source(char *srcbuf, int indent_level, struct rel_expr *expr) {
    return rel_to_source(srcbuf, indent_level, expr->rel);
}

char *rel_to_source(char *srcbuf, int indent_level, struct rel *rel) {
    switch (rel->type) {
        case LT:
            srcbuf = lt_rel_to_source(srcbuf, indent_level, rel->lt_rel);
            break;
        case LT_EQ:
            srcbuf = lteq_rel_to_source(srcbuf, indent_level, rel->lteq_rel);
            break;
        case ADD:
            srcbuf = add_rel_to_source(srcbuf, indent_level, rel->add_rel);
            break;
    };

    return srcbuf;
}

char *lt_rel_to_source(char *srcbuf, int indent_level, struct lt_rel *rel) {
    srcbuf = rel_to_source(srcbuf, indent_level, rel->rel);
    srcbuf = put(srcbuf, " < ");
    srcbuf = add_to_source(srcbuf, indent_level, rel->add);
    return srcbuf;
}

char *lteq_rel_to_source(char *srcbuf, int indent_level, struct lteq_rel *rel) {
    srcbuf = rel_to_source(srcbuf, indent_level, rel->rel);
    srcbuf = put(srcbuf, " <= ");
    srcbuf = add_to_source(srcbuf, indent_level, rel->add);
    return srcbuf;
}

char *add_rel_to_source(char *srcbuf, int indent_level, struct add_rel *rel) {
    return add_to_source(srcbuf, indent_level, rel->add);
}

char *add_to_source(char *srcbuf, int indent_level, struct add *add) {
    switch (add->type) {
        case PLUS:
            srcbuf = plus_add_to_source(srcbuf, indent_level, add->plus_add);
            break;
        case TERM:
            srcbuf = term_add_to_source(srcbuf, indent_level, add->term_add);
            break;
    };

    return srcbuf;
}

char *plus_add_to_source(char *srcbuf, int indent_level, struct plus_add *add) {
    srcbuf = add_to_source(srcbuf, indent_level, add->add);
    srcbuf = term_to_source(srcbuf, indent_level, add->term);
    return srcbuf;
}

char *term_add_to_source(char *srcbuf, int indent_level, struct term_add *add) {
    return term_to_source(srcbuf, indent_level, add->term);
}

char *term_to_source(char *srcbuf, int indent_level, struct term *term) {
    switch (term->type) {
        case MULT:
            srcbuf = mult_term_to_source(srcbuf, indent_level, term->mult_term);
            break;
        case FACTOR:
            srcbuf = factor_term_to_source(srcbuf, indent_level, term->factor_term);
            break;
    };

    return srcbuf;
}

char *mult_term_to_source(char *srcbuf, int indent_level, struct mult_term *term) {
    srcbuf = term_to_source(srcbuf, indent_level, term->term);
    srcbuf = put(srcbuf, " * ");
    srcbuf = factor_to_source(srcbuf, indent_level, term->factor);
    return srcbuf;
}

char *factor_term_to_source(char *srcbuf, int indent_level, struct factor_term *term) {
    return factor_to_source(srcbuf, indent_level, term->factor);
}

char *factor_to_source(char *srcbuf, int indent_level, struct factor *factor) {
    switch (factor->type) {
        case SUBEXPR:
            srcbuf = subexpr_factor_to_source(srcbuf, indent_level, factor->subexpr_factor);
            break;
        case NUM:
            srcbuf = num_factor_to_source(srcbuf, indent_level, factor->num_factor);
            break;
        case ID:
            srcbuf = id_factor_to_source(srcbuf, indent_level, factor->id_factor);
            break;
    };

    return srcbuf;
}

char *subexpr_factor_to_source(char *srcbuf, int indent_level, struct subexpr_factor *factor) {
    srcbuf = put(srcbuf, "(");
    srcbuf = expr_to_source(srcbuf, indent_level, factor->expr);
    srcbuf = put(srcbuf, ")");
    return srcbuf;
}

char *num_factor_to_source(char *srcbuf, int indent_level, struct num_factor *factor) {
    return put(srcbuf, factor->num);
}

char *id_factor_to_source(char *srcbuf, int indent_level, struct id_factor *factor) {
    return put(srcbuf, factor->id);
}
