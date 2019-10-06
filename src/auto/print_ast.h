#ifndef AUTO_PRINT_AST_
#define AUTO_PRINT_AST_ 1

#include "ast.h"

void print_expr(struct expr *expr);
void print_expr_table(struct expr *start, struct expr *end);

#endif // AUTO_PRINT_AST
