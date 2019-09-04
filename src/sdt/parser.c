#include <assert.h>
#include <stdio.h>
#include "parser.h"
#include "scanner.h"

struct parse_context *init_parse_context(char *input) {
    struct parse_context *context = malloc(*parse_context);
    assert(parse_context != NULL);

    struct scan_result *scan_result = token(input, empty_loc());
    context->lookahead = scan_result->token;
    context->input = scan_result->input;
    context->ast = context->parse_error = NULL;
    free(scan_result);

    return context;
}

void free_parse_context(struct parse_context *context) {
    free_token(context->lookahead);
    (*context->free_ast)(context->ast);
    free(context->parse_error);
    context->parse_error = context->ast = context->lookahead = NULL;
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
    struct token *token = NULL;
    if (peek(context, expected_token_type)) {
        struct scan_result scan_result = token(context->input, token_loc(context->lookahead));
        free_token(context->lookahead);
        token = context->lookahead = scan_result->token;
        context->input = scan_result->input;
        free(scan_result);
    } else {
        parse_error(context, expected_token_type);
    }

    return token;
}

struct parse_context *parse_error(struct parse_context *context, short expected_token_type) {
    struct token lookahead = context->lookahead;
    struct *parse_error = malloc(sizeof *parse_error);
    assert(parse_error != NULL);
    parse_error->loc = lookahead->loc;
    parse_error->expected = expected_token_type;
    parse_error->actual = lookahead->type;
    context->parse_error = parse_error;
    return context;
}

#define PARSE_ERROR_FMT "| Parse Error\n|\n| Got: %s\n| Expected: %s\n|\n| Line %d, Column %d\n|\n"

char *display_parse_error(struct parse_context *context) {
    struct parse_error *parse_error = context->parse_error;
    char *display_str;
    asprintf(&display_str, PARSE_ERROR_FMT,
        lexeme_for(parse_error->actual), lexeme_for(parse_error->expected),
        parse_error->loc->line, parse_error->loc->col);
    return display_str;
}

struct program *program(struct parse_context *context) {
    struct program *program = NULL;
    struct block *block = NULL;

    if (block = block(context)) {
        program = sast(context, init_program(block), free_program);
    }

    return program;
}

struct block *block(struct parse_context *context) {
    struct block *block = sast(context, init_block(), free_block());
    struct stmt *stmt = NULL;

    while (stmt = stmt(context)) {
        append_stmt(block, stmt);
    }

    return block;
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
        stmt = sast(context, init_stmt(stmt_type, stmt_val), free_stmt);
    }

    return stmt;
}

struct expr_stmt *expr_stmt(struct parse_context *context) {
    struct expr_stmt *expr_stmt = NULL;
    struct expr *expr = NULL;

    if (expr = expr(context)) {
        if (expect(context, ';')) {
            expr_stmt = sast(context, init_expr_stmt(expr), free_expr_stmt);
        }
    }

    return expr_stmt;
}

struct if_stmt *if_stmt(struct parse_context *context) {
    struct if_stmt *if_stmt = NULL;

    if (expect(context, T_IF) && expect(context, '(')) {
        struct expr *expr = NULL;
        if (expr = expr(context)) {
            if (expect(context, ')')) {
                struct stmt *stmt = NULL;
                if (stmt = stmt(context)) {
                    if_stmt = sast(context, init_if_stmt(expr, stmt), free_if_stmt);
                }
            }
        }
    }

    return if_stmt;
}

struct while_stmt *while_stmt(struct parse_context *context) {
    struct while_stmt *while_stmt = NULL;

    if (expect(context, T_WHILE) && expect(context, '(')) {
        struct expr *expr = NULL;
        if (expr = expr(context)) {
            if (expect(context, ')')) {
                struct stmt *stmt = NULL;
                if (stmt = stmt(context)) {
                    while_stmt = sast(context, init_while_stmt(expr, stmt), free_while_stmt);
                }
            }
        }
    }

    return while_stmt;
}

struct do_stmt *do_stmt(struct parse_context *context) {
    struct do_stmt *do_stmt = NULL;

    if (expect(context, T_DO)) {
        struct stmt *stmt = NULL;
        if (stmt = stmt(context)) {
            if (expect(context, T_WHILE) && expect(context, '(')) {
                struct expr *expr = NULL;
                if (expr = expr(context)) {
                    if (expect(context, ')') && expect(context, ';')) {
                        do_stmt = sast(context, init_do_stmt(stmt, expr), free_do_stmt);
                    }
                }
            }
        }
    }

    return do_stmt;
}

struct block_stmt *block_stmt(struct parse_context *context) {
    struct block_stmt *block_stmt = NULL;
    struct block *block = NULL;

    if (block = block(context)) {
        block_stmt = sast(context, init_block_stmt(block), free_block_stmt);
    }

    return block_stmt;
}

struct expr *expr(struct parse_context *context) {
    struct expr *expr = NULL;
    struct rel *rel = NULL;

    if (rel = rel(context)) {
        enum expr_type expr_type;
        void *expr_val expr_val;

        if (peek(context, '=')) {
            expect(context, '=');
            struct expr *rhs = NULL;

            if (rhs = expr(context)) {
                expr_type = ASSIGN;
                expr_val = sast(context, init_assign_expr(rel, expr), free_assign_expr);
            }
        } else {
            expr_type = REL;
            expr_val = sast(context, init_rel_expr(rel), free_rel_expr);
        }

        if (expr_val) {
            expr = sast(context, init_expr(expr_type, expr_val), free_expr);
        }
    }

    return expr;
}

struct rel *rel(struct parse_context *context) {
    struct rel *rel = NULL;
    struct add *add = NULL;

    if (add = add(context)) {
        rel = rel_rest(context, add);
    }

    return rel;
}

struct rel *rel_rest(struct parse_context *context, struct add *add) {
    enum rel_type = ADD;
    void *rel_val = sast(context, init_add_rel(add), free_add_rel);
    struct rel *rel = sast(context, init_rel(rel_type, rel_val), free_rel);

    while (true) {
        if (peek(context, '<')) {
            expect(context, '<');
            if (add = add(context)) {
                rel_type = LT;
                rel_val = sast(context, init_lt_rel(rel, add), free_lt_rel);
                rel = sast(context, init_rel(rel_type, rel), free_rel);
                continue;
            }
        } else if (peek(context, T_LT_EQ)) {
            expect(context, T_LT_EQ);
            if (add = add(context)) {
                rel_type = LT_EQ;
                rel_val = sast(context, init_lteq_rel(rel, add), free_lteq_rel);
                rel = sast(context, init_rel(rel_type, rel_val), free_rel);
                continue;
            }
        }
        break;
    };

    return rel;
}

struct add *add(struct parse_context *context) {
    struct add *add = NULL;
    struct term *term = NULL;

    if (term = term(context)) {
        add = add_rest(context, term);
    }

    return add;
}

struct add *add_rest(struct parse_context *context, struct term *term) {
    enum add_type = TERM;
    void *add_val = sast(context, init_term_add(term), free_term_add);
    struct add *add = sast(context, init_add(add_type, add_val), free_add);

    while (peek(context, '+')) {
        expect(context, '+');
        if (term = term(context)) {
            add_type = PLUS;
            add_val = sast(context, init_plus_add(add, term), free_plus_add);
            add = sast(context, init_add(add_type, add_val), free_add);
            continue;
        }
        break;
    };

    return add;
}

struct term *term(struct parse_context *context) {
    struct term *term = NULL;
    struct factor *factor = NULL;

    if (factor = factor(context)) {
        term = term_rest(context, factor);
    }

    return term;
}

struct term *term_rest(struct parse_context *context, struct factor *factor) {
    enum term_type = FACTOR;
    void *term_val = sast(context, init_factor_term(factor), free_factor_term);
    struct term *term = sast(context, init_term(term_type, term_val), free_term);

    while (peek(context, '*')) {
        expect(context, '*');
        if (factor = factor(context)) {
            term_type = MULT;
            term_val = sast(context, init_mult_term(term, factor), free_mult_term);
            term = sast(context, init_term(term_type, term_val), free_term);
            continue;
        }
        break;
    };

    return add;
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
        factor = sast(context, init_factor(factor_type, factor_val), free_factor);
    }

    return factor;
}

struct subexpr_factor *subexpr_factor(struct parse_context *context) {
    struct subexpr_factor *subexpr_factor = NULL;

    if (expect(context, '(')) {
        struct expr *expr = NULL;
        if (expr = expr(context)) {
            if (expect(context, ')')) {
                subexpr_factor = sast(context, init_subexpr_factor(expr), free_subexpr_factor);
            }
        }
    }

    return subexpr_factor;
}

struct num_factor *num_factor(struct parse_context *context) {
    struct num_factor *num_factor = NULL;
    struct token *token = NULL;

    if (token = peek(context, T_NUM)) {
        num_factor = sast(context, init_num_factor(token_val(token)), free_num_factor);
    }

    return num_factor;
}

struct id_factor *id_factor(struct parse_context *context) {
    struct id_factor *id_factor = NULL;
    struct token *token = NULL;

    if (token = peek(context, T_ID)) {
        id_factor = sast(context, init_id_factor(token_val(token)), free_id_factor);
    }

    return id_factor;
}

