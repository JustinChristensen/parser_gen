#ifndef AUTO_PRINT_AST_
#define AUTO_PRINT_AST_ 1

#include <regex/ast.h>

void print_expr(struct regex_expr *expr);
void print_expr_table(struct regex_expr *start, struct regex_expr *end);

#endif // AUTO_PRINT_AST
