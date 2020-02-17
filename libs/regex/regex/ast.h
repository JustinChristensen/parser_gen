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
    struct regex_error error;
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
struct expr tag_expr(char *tag);
struct expr char_expr(char ch);
struct expr dotall_expr();
struct expr empty_expr();

// expr context
struct expr_context expr_context(struct expr *exprbuf);
void sexpr(struct expr_context *context, struct expr expr);
struct expr *gexpr(struct expr_context *context);
union regex_result expr_to_result(struct expr_context *context);
void free_expr_context(struct expr_context *context);

// parse actions
bool noop_expr(union regex_result _, struct expr_context *context);
bool do_empty_expr(union regex_result _, struct expr_context *context);
bool do_alt_expr(union regex_result expr, struct expr_context *context);
bool do_cat_expr(union regex_result expr, struct expr_context *context);
bool do_sub_expr(union regex_result _, struct expr_context *context);
bool do_tag_expr(union regex_result tag, struct expr_context *context);
bool do_range(union regex_result range, struct expr_context *context);
bool do_char_class(union regex_result expr, struct expr_context *context);
bool do_neg_class(union regex_result expr, struct expr_context *context);
bool do_dotall_expr(union regex_result _, struct expr_context *context);
bool do_char_expr(union regex_result ch, struct expr_context *context);
bool do_star_expr(union regex_result _, struct expr_context *context);
bool do_plus_expr(union regex_result _, struct expr_context *context);
bool do_optional_expr(union regex_result _, struct expr_context *context);
bool do_repeat_exact_expr(union regex_result num, struct expr_context *context);

// parse action table
extern bool (*expr_actions[NUM_ACTIONS])(union regex_result val, void *context);
// parser interface
extern struct parse_interface expr_pinterface;

#endif // REGEX_AST_H_

