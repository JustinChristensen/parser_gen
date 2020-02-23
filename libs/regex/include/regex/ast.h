#ifndef REGEX_AST_H_
#define REGEX_AST_H_ 1

#include <regex/result_types.h>
#include <regex/parser.h>

struct regex_expr_context {
    struct regex_expr *bufstart;
    struct regex_expr *bufp;
    struct regex_expr *expr;
    bool has_error;
    struct regex_error error;
};

// expr context
struct regex_expr_context expr_context(struct regex_expr *exprbuf);
struct regex_expr *gexpr(struct regex_expr_context *context);
void free_expr_context(struct regex_expr_context *context);

// parser interface
extern struct regex_parse_interface const expr_parse_iface;

#endif // REGEX_AST_H_

