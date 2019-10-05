#ifndef AUTO_AST_H_
#define AUTO_AST_H_ 1

#include "parser.h"
#include "result_types.h"

#define EXPR_MAX 5000

struct expr_context {
    struct expr *exprbuf;
    struct expr *expr;
    bool has_error;
    struct parse_error error;
};

// expr constructors
struct expr alt_expr(struct expr *lexpr, struct expr *rexpr);
struct expr cat_expr(struct expr *lexpr, struct expr *rexpr);
struct expr star_expr(struct expr *expr);
struct expr plus_expr(struct expr *expr);
struct expr optional_expr(struct expr *expr);
struct expr sub_expr(struct expr *expr);
struct expr symbol_expr(char symbol);
struct expr dotall_expr();
struct expr empty_expr();

// expr context
struct expr_context expr_context(struct expr *exprbuf);
void sexpr(struct expr_context *context, struct expr expr);
struct expr *gexpr(struct expr_context *context);

// parse actions
void noop_expr(struct expr_context *context, struct expr *_);
void do_empty_expr(struct expr_context *context, struct expr *_);
void do_alt_expr(struct expr_context *context, struct expr *lexpr);
void do_cat_expr(struct expr_context *context, struct expr *lexpr);
void do_sub_expr(struct expr_context *context, struct expr *_);
void do_dotall_expr(struct expr_context *context, struct expr *_);
void do_symbol_expr(struct expr_context *context, char sym);
void do_star_expr(struct expr_context *context, struct expr *_);
void do_plus_expr(struct expr_context *context, struct expr *_);
void do_optional_expr(struct expr_context *context, struct expr *_);

// parse action table
void (*expr_actions[NUMACTIONS])(void *context, union rval lval);

#endif // AUTO_AST_H_

