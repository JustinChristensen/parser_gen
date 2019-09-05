#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <macros.h>
#include "ast.h"
#include "parser.h"
#include "scanner.h"

struct parse_context parse_context(char *input, bool debug) {
    struct scan_context scan_context_ = scan(scan_context(NULL, empty_loc(), input));

    struct parse_context context = {
        .scan_context = scan_context_,
        .lookahead = token(scan_context_),
        .ast = NULL,
        .error = NULL,
        .debug = debug,
    };

    return context;
}

void free_parse_context(struct parse_context *context) {
    free_token(context->lookahead);
    (*context->free_ast)(context->ast);
    free(context->error);
    context->error = context->ast = context->lookahead = NULL;
}

void *sast(struct parse_context *context, void *ast, void (*free_ast) (void *ast)) {
    context->ast = ast;
    context->free_ast = free_ast;
    return ast;
}

struct token *peek(struct parse_context *context, short token_type) {
    struct token *token = NULL;
    if (context->lookahead->type == token_type) {
        token = context->lookahead;
    }
    return token;
}

static void print_tokens(char *preamble, struct parse_context *context, short expected_token_type) {
    char *expected = lexeme_for(expected_token_type);
    printf("%s: expected -> '%s', got -> ", preamble, expected);
    free(expected);
    display_token(token(context->scan_context));
    printf("\n");
}

struct token *expect(struct parse_context *context, short expected_token_type) {
    struct token *next = NULL;
    struct scan_context scan_context = context->scan_context;

    if (peek(context, expected_token_type)) {
        if (context->debug) print_tokens("success", context, expected_token_type);
        scan_context = scan(scan_context);
        free_token(context->lookahead);
        next = context->lookahead = token(scan_context);
    } else {
        if (context->debug) print_tokens("failure", context, expected_token_type);
        parse_error(context, expected_token_type);
    }

    context->scan_context = scan_context;

    return next;
}

struct parse_context *parse_error(struct parse_context *context, short expected_token_type) {
    struct parse_error *parse_error = malloc(sizeof *parse_error);
    assert(parse_error != NULL);
    parse_error->loc = token_loc(context->lookahead);
    parse_error->expected = expected_token_type;
    parse_error->actual = token_type(context->lookahead);
    if (context->error) free(context->error);
    context->error = parse_error;

    return context;
}

#define PARSE_ERROR_FMT "| Parse Error\n|\n| Got: %s\n| Expected: %s\n|\n| Line %d, Column %d\n|\n"

void display_parse_error(struct parse_context *context) {
    struct parse_error *parse_error = context->error;
    char *actual = lexeme_for(parse_error->actual);
    char *expected = lexeme_for(parse_error->expected);
    fprintf(stderr, PARSE_ERROR_FMT, actual, expected,
        parse_error->loc.line, parse_error->loc.col);
    free(actual);
    free(expected);
}

struct program *program(struct parse_context *context) {
    struct program *program_ = NULL;
    struct block *block_ = NULL;

    if ((block_ = block(context)) && expect(context, T_EOF)) {
        program_ = sast(context, init_program(block_), VOIDFN1 free_program);
    }

    return program_;
}

struct block *block(struct parse_context *context) {
    struct block *block_ = NULL;

    if (expect(context, '{')) {
        struct stmt *stmt_ = NULL;
        block_ = sast(context, init_block(), VOIDFN1 free_block);

        while ((stmt_ = stmt(context))) {
            append_stmt(block_, stmt_);
        }

        if (context->error || !expect(context, '}')) {
            block_ = NULL;
        }
    }

    return block_;
}

struct stmt *stmt(struct parse_context *context) {
    struct stmt *stmt = NULL;
    enum stmt_type stmt_type;
    void *stmt_val = NULL;

    if (peek(context, T_IF)) {
        stmt_type = IF;
        stmt_val = if_stmt(context);
    } else if (peek(context, T_WHILE)) {
        stmt_type = WHILE;
        stmt_val = while_stmt(context);
    } else if (peek(context, T_DO)) {
        stmt_type = DO;
        stmt_val = do_stmt(context);
    } else if (peek(context, '{')) {
        stmt_type = BLOCK;
        stmt_val = block_stmt(context);
    } else {
        stmt_type = EXPR;
        stmt_val = expr_stmt(context);
    }

    if (stmt_val) {
        stmt = sast(context, init_stmt(stmt_type, stmt_val), VOIDFN1 free_stmt);
    }

    return stmt;
}

struct expr_stmt *expr_stmt(struct parse_context *context) {
    struct expr_stmt *expr_stmt_ = NULL;
    struct expr *expr_ = NULL;

    if ((expr_ = expr(context)) && expect(context, ';')) {
        expr_stmt_ = sast(context, init_expr_stmt(expr_), VOIDFN1 free_expr_stmt);
    }

    return expr_stmt_;
}

struct if_stmt *if_stmt(struct parse_context *context) {
    struct if_stmt *if_stmt_ = NULL;

    if (expect(context, T_IF) && expect(context, '(')) {
        struct expr *expr_ = NULL;
        if ((expr_ = expr(context)) && expect(context, ')')) {
            struct stmt *stmt_ = NULL;
            if ((stmt_ = stmt(context))) {
                if_stmt_ = sast(context, init_if_stmt(expr_, stmt_), VOIDFN1 free_if_stmt);
            }
        }
    }

    return if_stmt_;
}

struct while_stmt *while_stmt(struct parse_context *context) {
    struct while_stmt *while_stmt_ = NULL;

    if (expect(context, T_WHILE) && expect(context, '(')) {
        struct expr *expr_ = NULL;
        if ((expr_ = expr(context)) && expect(context, ')')) {
            struct stmt *stmt_ = NULL;
            if ((stmt_ = stmt(context))) {
                while_stmt_ = sast(context, init_while_stmt(expr_, stmt_), VOIDFN1 free_while_stmt);
            }
        }
    }

    return while_stmt_;
}

struct do_stmt *do_stmt(struct parse_context *context) {
    struct do_stmt *do_stmt_ = NULL;

    if (expect(context, T_DO)) {
        struct stmt *stmt_ = NULL;
        if ((stmt_ = stmt(context))) {
            if (expect(context, T_WHILE) && expect(context, '(')) {
                struct expr *expr_ = NULL;
                if ((expr_ = expr(context)) && expect(context, ')') && expect(context, ';')) {
                    do_stmt_ = sast(context, init_do_stmt(stmt_, expr_), VOIDFN1 free_do_stmt);
                }
            }
        }
    }

    return do_stmt_;
}

struct block_stmt *block_stmt(struct parse_context *context) {
    struct block_stmt *block_stmt_ = NULL;
    struct block *block_ = NULL;

    if ((block_ = block(context))) {
        block_stmt_ = sast(context, init_block_stmt(block_), VOIDFN1 free_block_stmt);
    }

    return block_stmt_;
}

struct expr *expr(struct parse_context *context) {
    struct expr *expr_ = NULL;
    struct rel *rel_ = NULL;

    if ((rel_ = rel(context))) {
        enum expr_type expr_type;
        void *expr_val = NULL;

        if (peek(context, '=') && expect(context, '=')) {
            struct expr *rhs = NULL;

            if ((rhs = expr(context))) {
                expr_type = ASSIGN;
                expr_val = sast(context, init_assign_expr(rel_, rhs), VOIDFN1 free_assign_expr);
            }
        } else {
            expr_type = REL;
            expr_val = sast(context, init_rel_expr(rel_), VOIDFN1 free_rel_expr);
        }

        if (expr_val) {
            expr_ = sast(context, init_expr(expr_type, expr_val), VOIDFN1 free_expr);
        }
    }

    return expr_;
}

struct rel *rel(struct parse_context *context) {
    struct rel *rel_ = NULL;
    struct add *add_ = NULL;

    if ((add_ = add(context))) {
        rel_ = rel_rest(context, add_);
    }

    return rel_;
}

struct rel *rel_rest(struct parse_context *context, struct add *add_) {
    enum rel_type rel_type = ADD;
    void *rel_val = sast(context, init_add_rel(add_), VOIDFN1 free_add_rel);
    struct rel *rel_ = sast(context, init_rel(rel_type, rel_val), VOIDFN1 free_rel);

    while (true) {
        if (peek(context, '<') && expect(context, '<')) {
            if ((add_ = add(context))) {
                rel_type = LT;
                rel_val = sast(context, init_lt_rel(rel_, add_), VOIDFN1 free_lt_rel);
                rel_ = sast(context, init_rel(rel_type, rel_val), VOIDFN1 free_rel);
                continue;
            }
        } else if (peek(context, T_LT_EQ) && expect(context, T_LT_EQ)) {
            if ((add_ = add(context))) {
                rel_type = LT_EQ;
                rel_val = sast(context, init_lteq_rel(rel_, add_), VOIDFN1 free_lteq_rel);
                rel_ = sast(context, init_rel(rel_type, rel_val), VOIDFN1 free_rel);
                continue;
            }
        }
        break;
    };

    return rel_;
}

struct add *add(struct parse_context *context) {
    struct add *add_ = NULL;
    struct term *term_ = NULL;

    if ((term_ = term(context))) {
        add_ = add_rest(context, term_);
    }

    return add_;
}

struct add *add_rest(struct parse_context *context, struct term *term_) {
    enum add_type add_type = TERM;
    void *add_val = sast(context, init_term_add(term_), VOIDFN1 free_term_add);
    struct add *add_ = sast(context, init_add(add_type, add_val), VOIDFN1 free_add);

    while (peek(context, '+') && expect(context, '+')) {
        if ((term_ = term(context))) {
            add_type = PLUS;
            add_val = sast(context, init_plus_add(add_, term_), VOIDFN1 free_plus_add);
            add_ = sast(context, init_add(add_type, add_val), VOIDFN1 free_add);
            continue;
        }
        break;
    };

    return add_;
}

struct term *term(struct parse_context *context) {
    struct term *term_ = NULL;
    struct factor *factor_ = NULL;

    if ((factor_ = factor(context))) {
        term_ = term_rest(context, factor_);
    }

    return term_;
}

struct term *term_rest(struct parse_context *context, struct factor *factor_) {
    enum term_type term_type = FACTOR;
    void *term_val = sast(context, init_factor_term(factor_), VOIDFN1 free_factor_term);
    struct term *term_ = sast(context, init_term(term_type, term_val), VOIDFN1 free_term);

    while (peek(context, '*') && expect(context, '*')) {
        if ((factor_ = factor(context))) {
            term_type = MULT;
            term_val = sast(context, init_mult_term(term_, factor_), VOIDFN1 free_mult_term);
            term_ = sast(context, init_term(term_type, term_val), VOIDFN1 free_term);
            continue;
        }
        break;
    };

    return term_;
}

struct factor *factor(struct parse_context *context) {
    struct factor *factor = NULL;
    enum factor_type factor_type;
    void *factor_val = NULL;

    if (peek(context, '(')) {
        factor_type = SUBEXPR;
        factor_val = subexpr_factor(context);
    } else if (peek(context, T_NUM)) {
        factor_type = NUM;
        factor_val = num_factor(context);
    } else if (peek(context, T_ID)) {
        factor_type = ID;
        factor_val = id_factor(context);
    }

    if (factor_val) {
        factor = sast(context, init_factor(factor_type, factor_val), VOIDFN1 free_factor);
    }

    return factor;
}

struct subexpr_factor *subexpr_factor(struct parse_context *context) {
    struct subexpr_factor *subexpr_factor_ = NULL;

    if (expect(context, '(')) {
        struct expr *expr_ = NULL;
        if ((expr_ = expr(context)) && expect(context, ')')) {
            subexpr_factor_ = sast(context, init_subexpr_factor(expr_), VOIDFN1 free_subexpr_factor);
        }
    }

    return subexpr_factor_;
}

struct num_factor *num_factor(struct parse_context *context) {
    struct num_factor *num_factor_ = NULL;
    struct token *token = NULL;

    if ((token = expect(context, T_NUM))) {
        num_factor_ = sast(context, init_num_factor(token_val(token)), VOIDFN1 free_num_factor);
    }

    return num_factor_;
}

struct id_factor *id_factor(struct parse_context *context) {
    struct id_factor *id_factor_ = NULL;
    struct token *token = NULL;

    if ((token = expect(context, T_ID))) {
        id_factor_ = sast(context, init_id_factor(token_val(token)), VOIDFN1 free_id_factor);
    }

    return id_factor_;
}

