#include <assert.h>
#include <stdio.h>
#include "parser.h"
#include "scanner.h"

bool peek(struct parse_context *context, short token_type) {
    return context->lookahead->type == token_type;
}

struct parse_context *expect(struct parse_context *context, short expected_token_type) {
    short actual_token_type = context->lookahead->type;

    if (actual_token_type == expected_token_type) {
        struct scan_result res = token(context->input);
        free_token(context->lookahead);
        context->lookahead = scan_result->token;
        context->input = scan_result->input;
        free(res);
    } else {
        context = parse_error(context, expected_token_type);
    }

    return context;
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

bool has_error(struct parse_context *context) {
    return context->parse_error != NULL;
}

struct program *program(struct parse_context *context) {
    struct program *program = NULL;
    struct block *block = NULL;

    if (block = block(context)) {
        program = init_program(block);
        context->ast = program;
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
        stmt = init_stmt(stmt_type, stmt_val);
        context->ast = stmt;
    }

    return stmt;
}

struct expr_stmt *expr_stmt(struct parse_context *context) {
}

struct if_stmt *if_stmt(struct parse_context *context) {
}

struct while_stmt *while_stmt(struct parse_context *context) {
}

struct do_stmt *do_stmt(struct parse_context *context) {
}

struct block_stmt *block_stmt(struct parse_context *context) {
}

struct expr *expr(struct parse_context *context) {
    struct expr *expr = NULL;
    struct rel *rel = NULL;

    if (rel = rel(context)) {
        enum expr_type expr_type;
        void *expr_val expr_val;

        if (peek(context, '=')) {
            expr_type = ASSIGN;
            expr_val = assign_expr(context);
        } else {
            expr_type = REL;
            expr_val = init_rel_expr(rel);
        }

        if (expr_val) {
            expr = init_expr(expr_type, expr_val);
            context->ast = expr;
        }
    }

    return expr;
}

struct assign_expr *assign_expr(struct parse_context *context) {
}

struct rel *rel(struct parse_context *context) {
    struct rel *rel = NULL;
    struct add *head = NULL;

    if (head = add(context)) {
        rel = rel_rest(context, head);
    }

    return rel;
}

struct rel *rel_rest(struct parse_context *context, struct add *head) {
    struct rel *rel = NULL;
    enum rel_type rel_type;
    void *rel_val = NULL;
    struct add *add = NULL;

    if (peek(context, '<')) {
        context = expect(context, '<');
        rel_type = LT;
        if (add = add(context)) {
            rel_val = init_lt_rel(head, add);
        }
        rel = rel_rest(context, head);
    } else if (peek(context, T_LT_EQ)) {
        context = expect(context, T_LT_EQ);
        rel_type = LT_EQ;
        if (add = add(context)) {
            rel_val = init_lteq_rel(, add);
        }
        rel = rel_rest(context, head);
    } else {
        rel_type = ADD;
        rel_val = init_add_rel(head);
    }

    if (rel_val) {
        rel = init_rel(rel_type, rel_val);
        context->ast = rel;
    }
}


struct add *add(struct parse_context *context) {
    struct add *add = NULL;
    enum add_type add_type;
    void *add_val = NULL;

    if (peek(context, '+')) {
        add_type = PLUS;
        add_val = plus_add(context);
    } else {
        add_type = TERM;
        add_val = term_add(context);
    }

    if (add_val) {
        add = init_add(add_type, add_val);
        context->ast = add;
    }

    return add;
}

struct plus_add *plus_add(struct parse_context *context) {
}

struct term_add *term_add(struct parse_context *context) {
}

struct term *term(struct parse_context *context) {
    struct term *term = NULL;
    enum term_type term_type;
    void *term_val = NULL;

    if (peek(context, '*')) {
        term_type = MULT;
        term_val = mult_term(context);
    } else {
        term_type = FACTOR;
        term_val = factor_term(context);
    }

    if (term_val) {
        term = init_term(term_type, term_val);
        context->ast = term;
    }

    return term;
}

struct mult_term *mult_term(struct parse_context *context) {
}

struct factor_term *factor_term(struct parse_context *context) {
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
        term = init_factor(factor_type, factor_val);
        context->ast = factor;
    }

    return factor;
}

struct subexpr_factor *subexpr_factor(struct parse_context *context) {
}

struct num_factor *num_factor(struct parse_context *context) {

}

struct id_factor *id_factor(struct parse_context *context) {

}

