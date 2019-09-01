#ifndef PARSER_H_
#define PARSER_H_

#include <stdlib.h>
#include <stdbool.h>
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
 *
 * Eliminating Left Recursion:
 * -------------
 * A: Aa
 *  | Ab
 *  | c
 * -------------
 * A: cR
 * R: aR
 *  | bR
 *  | ε
 * -------------
 * 1. rel: rel' '<' add
 *       | rel' '<=' add
 *       | add
 *
 *    A = rel
 *    a = '<' add
 *    b = '<=' add
 *    c = add
 *
 *    rel: add rel_rest
 *    rel_rest: '<' add rel_rest
 *            | '<= add rel_rest
 *            | ε
 *
 * 2. add: add' '+' term
 *       | term
 *
 *    A = add
 *    a = '+' term
 *    b = term
 *
 *    add: term add_rest
 *    add_rest: '+' term add_rest
 *            | ε
 *
 * 3. term: term' '*' factor
 *        | factor
 *
 *    A = term
 *    a = '*' factor
 *    b = factor
 *
 *    term: factor term_rest
 *    term_rest: '*' factor term_rest
 *             | ε
 */

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

struct parse_error {
    struct location loc;
    short expected;
    short actual;
};

struct parse_context {
    struct token *lookahead;
    char *input;
    void *ast;
    struct *parse_error;
};

/**
 * Parser
 *
 * Recursive Descent: Top down recursive parser
 * Predictive: Only needs to examine k tokens to make a parsing decision
 * LL(1): Because the above grammar only requires examining one token of lookahead
 *      - left to right, leftmost derivation
 */
struct program *program(struct parse_context *context);
struct block *block(struct parse_context *context);
struct stmt *stmt(struct parse_context *context);
struct expr_stmt *expr_stmt(struct parse_context *context);
struct if_stmt *if_stmt(struct parse_context *context);
struct while_stmt *while_stmt(struct parse_context *context);
struct do_stmt *do_stmt(struct parse_context *context);
struct block_stmt *block_stmt(struct parse_context *context);
struct expr *expr(struct parse_context *context);
struct assign_expr *assign_expr(struct parse_context *context);
struct rel *rel(struct parse_context *context);
struct rel *rel_rest(struct parse_context *context, struct add *head);
struct add *add(struct parse_context *context);
struct add *add_rest(struct parse_context *context, struct term *head);
struct term *term(struct parse_context *context);
struct term *term_rest(struct parse_context *context, struct factor *head);
struct factor *factor(struct parse_context *context);
struct subexpr_factor *subexpr_factor(struct parse_context *context);
struct num_factor *num_factor(struct parse_context *context);
struct id_factor *id_factor(struct parse_context *context);

// parser for instruction list
struct instruction_list *instructions(struct parse_context *context);

bool peek(struct parse_context *context, short token_type);
struct parse_context *expect(struct parse_context *context, short expected_token_type);
struct parse_context *parse_error(struct parse_context *context, short expected_token_type);
bool has_error(struct parse_context *context);
char *display_parse_error(struct *parse_error);
void free_parse_context(struct parse_context *parse_context);
void free_parse_error(struct parse_error *parse_error);

#endif // PARSER_H_

