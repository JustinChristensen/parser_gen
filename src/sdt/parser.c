#include <assert.h>
#include <stdio.h>
#include "ast.h"
#include "parser.h"
#include "scanner.h"

#define FREEFN (void (*) (void *))

struct parse_context *init_parse_context(char *input) {
    struct parse_context *context = malloc(sizeof *context);
    assert(context != NULL);

    struct scan_result *scan_result = token(input, empty_loc());
    context->lookahead = scan_result->token;
    context->input = scan_result->input;
    context->ast = context->error = NULL;
    free(scan_result);

    return context;
}

void free_parse_context(struct parse_context *context) {
    free_token(context->lookahead);
    (*context->free_ast)(context->ast);
    free(context->error);
    context->error = context->ast = context->lookahead = NULL;
    free(context);
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

struct token *expect(struct parse_context *context, short expected_token_type) {
    struct token *next = NULL;

    if (peek(context, expected_token_type)) {
        struct scan_result *scan_result = token(context->input, token_loc(context->lookahead));
        free_token(context->lookahead);
        next = context->lookahead = scan_result->token;
        context->input = scan_result->input;
        free(scan_result);
    } else {
        parse_error(context, expected_token_type);
    }

    return next;
}

struct parse_context *parse_error(struct parse_context *context, short expected_token_type) {
    struct token *lookahead = context->lookahead;
    struct parse_error *parse_error = malloc(sizeof *parse_error);
    assert(parse_error != NULL);
    parse_error->loc = lookahead->loc;
    parse_error->expected = expected_token_type;
    parse_error->actual = lookahead->type;
    context->error = parse_error;
    return context;
}

#define PARSE_ERROR_FMT "| Parse Error\n|\n| Got: %s\n| Expected: %s\n|\n| Line %d, Column %d\n|\n"

char *display_parse_error(struct parse_context *context) {
    struct parse_error *parse_error = context->error;
    char *display_str;
    asprintf(&display_str, PARSE_ERROR_FMT,
        lexeme_for(parse_error->actual), lexeme_for(parse_error->expected),
        parse_error->loc.line, parse_error->loc.col);
    return display_str;
}

struct program *program(struct parse_context *context) {
    struct program *program_ = NULL;
    struct block *block_ = NULL;

    if ((block_ = block(context))) {
        program_ = sast(context, init_program(block_), FREEFN free_program);
    }

    return program_;
}

struct block *block(struct parse_context *context) {
    struct block *block_ = sast(context, init_block(), FREEFN free_block);
    struct stmt *stmt_ = NULL;

    while ((stmt_ = stmt(context))) {
        append_stmt(block_, stmt_);
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
        stmt = sast(context, init_stmt(stmt_type, stmt_val), FREEFN free_stmt);
    }

    return stmt;
}

struct expr_stmt *expr_stmt(struct parse_context *context) {
    struct expr_stmt *expr_stmt_ = NULL;
    struct expr *expr_ = NULL;

    if ((expr_ = expr(context)) && expect(context, ';')) {
        expr_stmt_ = sast(context, init_expr_stmt(expr_), FREEFN free_expr_stmt);
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
                if_stmt_ = sast(context, init_if_stmt(expr_, stmt_), FREEFN free_if_stmt);
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
                while_stmt_ = sast(context, init_while_stmt(expr_, stmt_), FREEFN free_while_stmt);
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
                    do_stmt_ = sast(context, init_do_stmt(stmt_, expr_), FREEFN free_do_stmt);
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
        block_stmt_ = sast(context, init_block_stmt(block_), FREEFN free_block_stmt);
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
                expr_val = sast(context, init_assign_expr(rel_, expr_), FREEFN free_assign_expr);
            }
        } else {
            expr_type = REL;
            expr_val = sast(context, init_rel_expr(rel_), FREEFN free_rel_expr);
        }

        if (expr_val) {
            expr_ = sast(context, init_expr(expr_type, expr_val), FREEFN free_expr);
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
    void *rel_val = sast(context, init_add_rel(add_), FREEFN free_add_rel);
    struct rel *rel_ = sast(context, init_rel(rel_type, rel_val), FREEFN free_rel);

    while (true) {
        if (peek(context, '<') && expect(context, '<')) {
            if ((add_ = add(context))) {
                rel_type = LT;
                rel_val = sast(context, init_lt_rel(rel_, add_), FREEFN free_lt_rel);
                rel_ = sast(context, init_rel(rel_type, rel_), FREEFN free_rel);
                continue;
            }
        } else if (peek(context, T_LT_EQ) && expect(context, T_LT_EQ)) {
            if ((add_ = add(context))) {
                rel_type = LT_EQ;
                rel_val = sast(context, init_lteq_rel(rel_, add_), FREEFN free_lteq_rel);
                rel_ = sast(context, init_rel(rel_type, rel_val), FREEFN free_rel);
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
    void *add_val = sast(context, init_term_add(term_), FREEFN free_term_add);
    struct add *add_ = sast(context, init_add(add_type, add_val), FREEFN free_add);

    while (peek(context, '+') && expect(context, '+')) {
        if ((term_ = term(context))) {
            add_type = PLUS;
            add_val = sast(context, init_plus_add(add_, term_), FREEFN free_plus_add);
            add_ = sast(context, init_add(add_type, add_val), FREEFN free_add);
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
    void *term_val = sast(context, init_factor_term(factor_), FREEFN free_factor_term);
    struct term *term_ = sast(context, init_term(term_type, term_val), FREEFN free_term);

    while (peek(context, '*') && expect(context, '*')) {
        if ((factor_ = factor(context))) {
            term_type = MULT;
            term_val = sast(context, init_mult_term(term_, factor_), FREEFN free_mult_term);
            term_ = sast(context, init_term(term_type, term_val), FREEFN free_term);
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
    } else {
        factor_type = ID;
        factor_val = id_factor(context);
    }

    if (factor_val) {
        factor = sast(context, init_factor(factor_type, factor_val), FREEFN free_factor);
    }

    return factor;
}

struct subexpr_factor *subexpr_factor(struct parse_context *context) {
    struct subexpr_factor *subexpr_factor_ = NULL;

    if (expect(context, '(')) {
        struct expr *expr_ = NULL;
        if ((expr_ = expr(context)) && expect(context, ')')) {
            subexpr_factor_ = sast(context, init_subexpr_factor(expr_), FREEFN free_subexpr_factor);
        }
    }

    return subexpr_factor_;
}

struct num_factor *num_factor(struct parse_context *context) {
    struct num_factor *num_factor_ = NULL;
    struct token *token = NULL;

    if ((token = peek(context, T_NUM))) {
        num_factor_ = sast(context, init_num_factor(*((long*) token_val(token))), FREEFN free_num_factor);
    }

    return num_factor_;
}

struct id_factor *id_factor(struct parse_context *context) {
    struct id_factor *id_factor_ = NULL;
    struct token *token = NULL;

    if ((token = peek(context, T_ID))) {
        id_factor_ = sast(context, init_id_factor(token_val(token)), FREEFN free_id_factor);
    }

    return id_factor_;
}

