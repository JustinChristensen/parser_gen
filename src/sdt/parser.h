#ifndef PARSER_H_
#define PARSER_H_

#include "scanner.h"

/**
 * Grammar:
 * 
 * program
 *     : block
 * block
 *     : '{' stmts '}'
 * stmts
 *     : stmts' stmt
 *     | ε 
 * stmt
 *     : expr ';'
 *     | "if" '(' expr ')' stmt'
 *     | "while" '(' expr ')' stmt'
 *     | "do" stmt' "while" '(' expr ')' ';'
 *     | block
 * expr
 *     : rel '=' expr'     <-- does assignment to a rel make sense?
 *     | rel
 * rel
 *     : rel' ',' add
 *     | rel' '<=' add
 *     | add
 * add
 *     : add' '+' term
 *     | term
 * term
 *     : term' '*' factor
 *     | factor
 * factor
 *     : '(' expr ')'
 *     | num
 *     | id
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

struct block {
    struct stmt *head;
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
    struct token *lookahead;
    char *input;
};

// TODO: look into how to implement C return type polymophism
// the the recursive descent parser could then take a function pointer argument
struct program *parse_syn(struct parse_context *context);
struct instruction_list *parse_inst(struct parse_context *context);

#endif // PARSER_H_

