#ifndef REGEX_AST_H_
#define REGEX_AST_H_ 1

#include "result_types.h"
#include "parser.h"

#define EXPR_MAX 5000

struct expr_context {
    struct expr *bufstart;
    struct expr *bufp;
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
struct expr repeat_exact_expr(int num, struct expr *expr);
struct expr sub_expr(struct expr *expr);
struct expr range_expr(struct expr *next, struct char_range range);
struct expr char_class_expr(struct expr *ranges);
struct expr id_expr(char *id);
struct expr symbol_expr(char symbol);
struct expr dotall_expr();
struct expr empty_expr();

// expr context
struct expr_context expr_context(struct expr *exprbuf);
void sexpr(struct expr_context *context, struct expr expr);
struct expr *gexpr(struct expr_context *context);
union regex_result expr_to_rval(struct expr_context *context);
void free_expr_context(struct expr_context *context);

// parse actions
void noop_expr(struct expr_context *context, union regex_result _);
void do_empty_expr(struct expr_context *context, union regex_result _);
void do_alt_expr(struct expr_context *context, union regex_result expr);
void do_cat_expr(struct expr_context *context, union regex_result expr);
void do_sub_expr(struct expr_context *context, union regex_result _);
void do_id_expr(struct expr_context *context, union regex_result id);
void do_range(struct expr_context *context, union regex_result range);
void do_char_class(struct expr_context *context, union regex_result expr);
void do_neg_class(struct expr_context *context, union regex_result expr);
void do_dotall_expr(struct expr_context *context, union regex_result _);
void do_symbol_expr(struct expr_context *context, union regex_result sym);
void do_star_expr(struct expr_context *context, union regex_result _);
void do_plus_expr(struct expr_context *context, union regex_result _);
void do_optional_expr(struct expr_context *context, union regex_result _);
void do_repeat_exact_expr(struct expr_context *context, union regex_result num);

// parse action table
extern void (*expr_actions[NUM_ACTIONS])(void *context, union regex_result lval);

#endif // REGEX_AST_H_

