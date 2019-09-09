#ifndef SDT_SOURCE_H_
#define SDT_SOURCE_H_ 1

#include <stdlib.h>
#include "ast.h"

#define TOSOURCEFN (char *(*) (char *, int, void *))

void print_source(FILE *handle, void *ast, char *(*to_source) (char *srcbuf, int indent_level, void *ast));
char *program_to_source(char *srcbuf, int indent_level, struct program *program);
char *block_to_source(char *srcbuf, int indent_level, struct block *block);
char *stmt_to_source(char *srcbuf, int indent_level, struct stmt *stmt);
char *expr_stmt_to_source(char *srcbuf, int indent_level, struct expr_stmt *stmt);
char *if_stmt_to_source(char *srcbuf, int indent_level, struct if_stmt *stmt);
char *while_stmt_to_source(char *srcbuf, int indent_level, struct while_stmt *stmt);
char *do_stmt_to_source(char *srcbuf, int indent_level, struct do_stmt *stmt);
char *block_stmt_to_source(char *srcbuf, int indent_level, struct block_stmt *stmt);
char *expr_to_source(char *srcbuf, int indent_level, struct expr *expr);
char *assign_expr_to_source(char *srcbuf, int indent_level, struct assign_expr *expr);
char *rel_expr_to_source(char *srcbuf, int indent_level, struct rel_expr *expr);
char *rel_to_source(char *srcbuf, int indent_level, struct rel *rel);
char *lt_rel_to_source(char *srcbuf, int indent_level, struct lt_rel *rel);
char *lteq_rel_to_source(char *srcbuf, int indent_level, struct lteq_rel *rel);
char *add_rel_to_source(char *srcbuf, int indent_level, struct add_rel *rel);
char *add_to_source(char *srcbuf, int indent_level, struct add *add);
char *plus_add_to_source(char *srcbuf, int indent_level, struct plus_add *add);
char *term_add_to_source(char *srcbuf, int indent_level, struct term_add *add);
char *term_to_source(char *srcbuf, int indent_level, struct term *term);
char *mult_term_to_source(char *srcbuf, int indent_level, struct mult_term *term);
char *factor_term_to_source(char *srcbuf, int indent_level, struct factor_term *term);
char *factor_to_source(char *srcbuf, int indent_level, struct factor *factor);
char *subexpr_factor_to_source(char *srcbuf, int indent_level, struct subexpr_factor *factor);
char *num_factor_to_source(char *srcbuf, int indent_level, struct num_factor *factor);
char *id_factor_to_source(char *srcbuf, int indent_level, struct id_factor *factor);

#endif // SDT_SOURCE_H_
