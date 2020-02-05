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
union regex_result expr_to_result(struct expr_context *context);
void free_expr_context(struct expr_context *context);

// parse actions
bool noop_expr(union regex_result _, struct parse_context *context);
bool do_empty_expr(union regex_result _, struct parse_context *context);
bool do_alt_expr(union regex_result expr, struct parse_context *context);
bool do_cat_expr(union regex_result expr, struct parse_context *context);
bool do_sub_expr(union regex_result _, struct parse_context *context);
bool do_id_expr(union regex_result id, struct parse_context *context);
bool do_range(union regex_result range, struct parse_context *context);
bool do_char_class(union regex_result expr, struct parse_context *context);
bool do_neg_class(union regex_result expr, struct parse_context *context);
bool do_dotall_expr(union regex_result _, struct parse_context *context);
bool do_symbol_expr(union regex_result sym, struct parse_context *context);
bool do_star_expr(union regex_result _, struct parse_context *context);
bool do_plus_expr(union regex_result _, struct parse_context *context);
bool do_optional_expr(union regex_result _, struct parse_context *context);
bool do_repeat_exact_expr(union regex_result num, struct parse_context *context);

// parse action table
extern bool (*expr_actions[NUM_ACTIONS])(union regex_result val, struct parse_context *context);

#endif // REGEX_AST_H_

