#include <assert.h>
#include <stdio.h>
#include "parser.h"
#include "scanner.h"

void *sast(struct parse_context *context, void *ast) {
    context->ast = ast;
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
        struct scan_result res = token(context->input);
        free_token(context->lookahead);
        token = context->lookahead = scan_result->token;
        context->input = scan_result->input;
        free(res);
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

char *display_parse_error(struct parse_error *parse_error) {
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
        program = sast(context, init_program(block));
    }

    return program;
}

struct block *block(struct parse_context *context) {
    // context = expect(context, '{');
    // struct block *block = init_block();
    // struct stmt *st;
    // while (st = stmt(context)) {
    //     block = append_stmt(block, st);
    // }
    // context = expect(context, '}');
    // return block;
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
        stmt = sast(context, init_stmt(stmt_type, stmt_val));
    }

    return stmt;
}

struct expr_stmt *expr_stmt(struct parse_context *context) {
    struct expr_stmt *expr_stmt = NULL;
    struct expr *expr = NULL;

    if (expr = expr(context)) {
        if (expect(context, ';')) {
            expr_stmt = sast(context, init_expr_stmt(expr));
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
                    if_stmt = sast(context, init_if_stmt(expr, stmt));
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
                    while_stmt = sast(context, init_while_stmt(expr, stmt));
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
                        do_stmt = sast(context, init_do_stmt(stmt, expr));
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
        block_stmt = sast(context, init_block_stmt(block));
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
                expr_val = sast(context, init_assign_expr(rel, expr));
            }
        } else {
            expr_type = REL;
            expr_val = sast(context, init_rel_expr(rel));
        }

        if (expr_val) {
            expr = sast(context, init_expr(expr_type, expr_val));
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
    void *rel_val = sast(context, init_add_rel(add));
    struct rel *rel = sast(context, init_rel(rel_type, rel_val));

    while (true) {
        if (peek(context, '<')) {
            expect(context, '<');
            if (add = add(context)) {
                rel_type = LT;
                rel_val = sast(context, init_lt_rel(rel, add));
                rel = sast(context, init_rel(rel_type, rel));
                continue;
            }
        } else if (peek(context, T_LT_EQ)) {
            expect(context, T_LT_EQ);
            if (add = add(context)) {
                rel_type = LT_EQ;
                rel_val = sast(context, init_lteq_rel(rel, add));
                rel = sast(context, init_rel(rel_type, rel_val));
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
    void *add_val = sast(context, init_term_add(term));
    struct add *add = sast(context, init_add(add_type, add_val));

    while (peek(context, '+')) {
        expect(context, '+');
        if (term = term(context)) {
            add_type = PLUS;
            add_val = sast(context, init_plus_add(add, term));
            add = sast(context, init_add(add_type, add_val));
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
    void *term_val = sast(context, init_factor_term(factor));
    struct term *term = sast(context, init_term(term_type, term_val));

    while (peek(context, '*')) {
        expect(context, '*');
        if (factor = factor(context)) {
            term_type = MULT;
            term_val = sast(context, init_mult_term(term, factor));
            term = sast(context, init_term(term_type, term_val));
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
        factor = sast(context, init_factor(factor_type, factor_val));
    }

    return factor;
}

struct subexpr_factor *subexpr_factor(struct parse_context *context) {
    struct subexpr_factor *subexpr_factor = NULL;

    if (expect(context, '(')) {
        struct expr *expr = NULL;
        if (expr = expr(context)) {
            if (expect(context, ')')) {
                subexpr_factor = sast(context, init_subexpr_factor(expr));
            }
        }
    }

    return subexpr_factor;
}

struct num_factor *num_factor(struct parse_context *context) {
    struct num_factor *num_factor = NULL;
    struct token *token = NULL;

    if (token = peek(context, T_NUM)) {
        num_factor = sast(context, init_num_factor(token_val(token)));
    }

    return num_factor;
}

struct id_factor *id_factor(struct parse_context *context) {
    struct id_factor *id_factor = NULL;
    struct token *token = NULL;

    if (token = peek(context, T_ID)) {
        id_factor = sast(context, init_id_factor(token_val(token)));
    }

    return id_factor;
}

