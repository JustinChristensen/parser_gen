#ifndef PARSER_H_
#define PARSER_H_

#include <stdlib.h>
#include "scanner.h"

/**
 * Translation Scheme:
 * https://en.wikipedia.org/wiki/Syntax-directed_translation
 *
 * Grammar                                      Semantic Actions (for Syntax Tree)
 *
 * program
 *     : block                                  { program(block) }
 * block
 *     : '{' stmts '}'                          { block(stmts) }
 * stmts
 *     : stmts' stmt
 *     | ε
 * stmt
 *     : expr ';'                               { expr_stmt(expr) }
 *     | "if" '(' expr ')' stmt'                { if_stmt(expr, stmt') }
 *     | "while" '(' expr ')' stmt'             { while_stmt(expr, stmt') }
 *     | "do" stmt' "while" '(' expr ')' ';'    { do_stmt(stmt', expr) }
 *     | block                                  { block_stmt(block) }
 *
 * ↓ does assignment to a rel make sense?
 * expr
 *     : rel '=' expr'                          { assign_expr(rel, expr') }
 *     | rel                                    { rel_expr(rel) }
 * rel
 *     : rel' '<' add                           { lt_rel(rel', add) }
 *     | rel' '<=' add                          { lteq_rel(rel', add) }
 *     | add                                    { add_rel(add) }
 * add
 *     : add' '+' term                          { plus_add(add', term) }
 *     | term                                   { term_add(term) }
 * term
 *     : term' '*' factor                       { mult_term(term', factor) }
 *     | factor                                 { factor_term(factor) }
 * factor
 *     : '(' expr ')'                           { subexpr_factor(expr) }
 *     | num                                    { num_factor(num) }
 *     | id                                     { id_factor(id) }
 */

/**
 * Syntax Tree:
 *
 * This syntax tree is overtly verbose just for demonstration purposes, and closely matches
 * the grammar above. The number of ast node types could be dramatically reduced based on our semantic needs.
 */
struct program {
    struct block *block;
};

#define MAX_STATEMENTS 128
struct block {
    struct stmt *stmts[MAX_STATEMENTS];
    size_t size;
};

enum stmt_type {
    EXPR,
    IF,
    WHILE,
    DO
};

// tagged union
struct stmt {
    enum stmt_type type;
    union {
        struct expr_stmt *expr_stmt;
        struct if_stmt *if_stmt;
        struct while_stmt *while_stmt;
        struct do_stmt *do_stmt;
        struct block_stmt *block_stmt;
    };
};

struct expr_stmt {
    struct expr *expr;
};

struct if_stmt {
    struct expr *expr;
    struct stmt *stmt;
};

struct while_stmt {
    struct expr *expr;
    struct stmt *stmt;
};

struct do_stmt {
    struct stmt *stmt;
    struct expr *expr;
};

struct block_stmt {
    struct block *block;
};

enum expr_type {
    ASSIGN,
    REL
};

struct expr {
    enum expr_type type;
    union {
        struct assign_expr *assign_expr;
        struct rel_expr *rel_expr;
    };
};

struct assign_expr {
    struct rel *rel;
    struct expr *expr;
};

struct rel_expr {
    struct rel *rel;
};

enum rel_type {
    LT,
    LT_EQ,
    ADD
};

struct rel {
    enum rel_type type;
    union {
        struct lt_rel *lt_rel;
        struct lteq_rel *lteq_rel;
        struct add_rel *add_rel;
    };
};

struct lt_rel {
    struct rel *rel;
    struct add *add;
};

struct lteq_rel {
    struct rel *rel;
    struct add *add;
};

struct add_rel {
    struct add *add;
};

enum add_type {
    PLUS,
    TERM
};

struct add {
    enum add_type type;
    union {
        struct plus_add *plus_add;
        struct term_add *term_add;
    };
};

struct plus_add {
    struct add *add;
    struct term *term;
};

struct term_add {
    struct term *term;
};

enum term_type {
    MULT,
    FACTOR
};

struct term {
    enum term_type type;
    union {
        struct mult_term *mult_term;
        struct factor_term *factor_term;
    };
};

struct mult_term {
    struct term *term;
    struct factor *factor;
};

struct factor_term {
    struct factor *factor;
};

enum factor_type {
    SUBEXPR,
    NUM,
    ID
};

struct factor {
    enum factor_type type;
    union {
        struct subexpr_factor *subexpr_factor;
        struct num_factor *num_factor;
        struct id_factor *id_factor;
    };
};

struct subexpr_factor {
    struct expr *expr;
};

struct num_factor {
    int num;
};

struct id_factor {
    char *id;
};

/**
 * Three Address Code Instruction
 */
enum instruction_type {
    LABEL_INST,
    ARRAY_SET_INST,
    ARRAY_GET_INST,
    ASSIGN_INST,
    COND_GOTO_INST,
    GOTO_INST
};

struct instruction {
    enum instruction_type type;
    union {
        // TODO: structures for the above types
    };
};

struct instruction_list {
    struct instruction *head;
    size_t size;
};

struct parse_context {
    char *input;
    struct token *lookahead;
};

struct parse_context *expect(struct parse_context *context, short token_type);

// ast lifecycle functions
struct program        *init_program(struct block *block);
struct block          *init_block();
struct block          *append_stmt(struct block *block, struct stmt *stmt);
struct stmt           *init_stmt(enum stmt_type type, void *val);
struct expr_stmt      *init_expr_stmt(struct expr *expr);
struct if_stmt        *init_if_stmt(struct expr *expr, struct stmt *stmt);
struct while_stmt     *init_while_stmt(struct expr *expr, struct stmt *stmt);
struct do_stmt        *init_do_stmt(struct stmt *stmt, struct expr *expr);
struct block_stmt     *init_block_stmt(struct block *block);
struct expr           *init_expr(enum expr_type type, void *val);
struct assign_expr    *init_assign_expr(struct rel *rel, struct expr *expr);
struct rel_expr       *init_rel_expr(struct rel *rel);
struct rel            *init_rel(enum rel_type type, void *val);
struct lt_rel         *init_lt_rel(struct rel *rel, struct add *add);
struct lteq_rel       *init_lteq_rel(struct rel *rel, struct add *add);
struct add_rel        *init_add_rel(struct add *add);
struct add            *init_add(enum add_type type, void *val);
struct plus_add       *init_plus_add(struct add *add, struct term *term);
struct term_add       *init_term_add(struct term *term);
struct term           *init_term(enum term_type type, void *val);
struct mult_term      *init_mult_term(struct term *term, struct factor *factor);
struct factor_term    *init_factor_term(struct factor *factor);
struct factor         *init_factor(enum factor_type type, void *val);
struct subexpr_factor *init_subexpr_factor(struct expr *expr);
struct num_factor     *init_num_factor(int num);
struct id_factor      *init_id_factor(char *id);

void free_program(struct program *program);
void free_block(struct block *block);
void free_stmt(struct stmt *stmt);
void free_expr_stmt(struct expr_stmt *stmt);
void free_if_stmt(struct if_stmt *stmt);
void free_while_stmt(struct while_stmt *stmt);
void free_do_stmt(struct do_stmt *stmt);
void free_block_stmt(struct block_stmt *stmt);
void free_expr(struct expr *expr);
void free_assign_expr(struct assign_expr *expr);
void free_rel_expr(struct rel_expr *expr);
void free_rel(struct rel *rel);
void free_lt_rel(struct lt_rel *rel);
void free_lteq_rel(struct lteq_rel *rel);
void free_add_rel(struct add_rel *rel);
void free_add(struct add *add);
void free_plus_add(struct plus_add *add);
void free_term_add(struct term_add *add);
void free_term(struct term *term);
void free_mult_term(struct mult_term *term);
void free_factor_term(struct factor_term *term);
void free_factor(struct factor *factor);
void free_subexpr_factor(struct subexpr_factor *factor);
void free_num_factor(struct num_factor *factor);
void free_id_factor(struct id_factor *factor);

// parser for syntax tree
struct program        *program(struct parse_context *context);
struct block          *block(struct parse_context *context);
struct stmt           *stmt(struct parse_context *context);
struct expr_stmt      *expr_stmt(struct parse_context *context);
struct if_stmt        *if_stmt(struct parse_context *context);
struct while_stmt     *while_stmt(struct parse_context *context);
struct do_stmt        *do_stmt(struct parse_context *context);
struct block_stmt     *block_stmt(struct parse_context *context);
struct expr           *expr(struct parse_context *context);
struct assign_expr    *assign_expr(struct parse_context *context);
struct rel_expr       *rel_expr(struct parse_context *context);
struct rel            *rel(struct parse_context *context);
struct lt_rel         *lt_rel(struct parse_context *context);
struct lteq_rel       *lteq_rel(struct parse_context *context);
struct add_rel        *add_rel(struct parse_context *context);
struct add            *add(struct parse_context *context);
struct plus_add       *plus_add(struct parse_context *context);
struct term_add       *term_add(struct parse_context *context);
struct term           *term(struct parse_context *context);
struct mult_term      *mult_term(struct parse_context *context);
struct factor_term    *factor_term(struct parse_context *context);
struct factor         *factor(struct parse_context *context);
struct subexpr_factor *subexpr_factor(struct parse_context *context);
struct num_factor     *num_factor(struct parse_context *context);
struct id_factor      *id_factor(struct parse_context *context);

// parser for instruction list
struct instruction_list *instructions(struct parse_context *context);

#endif // PARSER_H_

