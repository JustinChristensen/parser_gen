#include <stdbool.h>
#include <ctype.h>
#include "parser.h"
#include "ast.h"

struct scan_context scan_context(char *input) {
    return (struct scan_context) {
        .input = input,
        .input_col = 1,
        .token = 0,
        .token_col = 1
    };
}

struct scan_context scan(struct scan_context context) {
    while (isblank(*context.input)) {
        context.input_col++;
        context.input++;
    }

    context.token_col = context.input_col;

    if (*context.input == '*') {
        context.token = STAR;
        context.input_col++;
    } else if (*context.input == '|') {
        context.token = ALT;
        context.input_col++;
    } else {
        if (*context.input == '\\') {
            context.input++;
            context.input_col++;
        }

        context.token = *context.input++;
        context.input_col++;
    }

    return context;
}

char token(struct scan_context context) {
    return context.token;
}

int token_col(struct scan_context context) {
    return context.token_col;
}

struct parse_context parse_context(char *input) {
    struct scan_context scontext = scan(scan_context(input));

    return (struct parse_context) {
        .scan_context = scontext,
        .lookahead = token(scontext),
        .lookahead_col = token_col(scontext),
        .has_error = false,
        .expr = empty_expr(),
        .error = (struct parse_error) { 0, 0, 0 }
    };
}

int peek(struct parse_context *context, int expected, bool (*is) (int c)) {
    int actual = context->lookahead;

    if ((is && !is(actual)) || actual != expected) {
        actual = '\0';
    }

    return actual;
}

int expect(struct parse_context *context, int expected, bool (*is) (int c)) {
    if (peek(context, expected, is)) {
        struct scan_context scan_context = scan(context->scan_context);
        context->scan_context = scan_context;
        context->lookahead = token(scan_context);
        context->lookahead_col = token_col(scan_context);
    } else {
        context->has_error = true;
        context->error = (struct parse_error) {
            .actual = context->lookahead,
            .token_col = context->lookahead_col,
            .expected = expected
        };
        expected = '\0';
    }

    return expected;
}

bool is_tail_terminal(struct parse_context *context) {
    return
        peek(context, ALT, NULL) ||
        peek(context, STAR, NULL) ||
        peek(context, '\0', NULL);
}

bool parse_expr(struct parse_context *context) {
    bool success = is_tail_terminal(context);

    if (peek(context, LPAREN, NULL)) {
        expect(context, LPAREN, NULL);
        if (parse_expr(context) && expect(context, RPAREN, NULL)) {
            sexpr(context, sub_expr(gexpr(context)));
            success = true;
        }
    } else {
        int sym = peek(context, PRINTABLE, isprint);

        expect(context, PRINTABLE, isprint);

        if (sym) {
            success = true;
            sexpr(context, symbol_expr((char) sym));
        }
    }

    if (success) {
        while (true) {
            struct expr lexpr = gexpr(context);

            if (peek(context, ALT, NULL)) {
                expect(context, ALT, NULL);
                if (parse_expr(context)) {
                    sexpr(context, alt_expr(lexpr, gexpr(context)));
                }
            } else if (parse_expr(context)) {
                sexpr(context, cat_expr(lexpr, gexpr(context)));
            } else if (peek(context, STAR, NULL)) {
                expect(context, STAR, NULL);
                sexpr(context, star_expr(lexpr);
            } else {
                break;
            }
        }
    }

    return success;
}

void sexpr(struct parse_context *context, struct expr expr) {
    context->expr = expr;
}

struct expr gexpr(struct parse_context *context) {
    return context->expr;
}

bool has_error(struct parse_context *context) {
    return context->has_error;
}

struct parse_error gerror(struct parse_context *context) {
    return context->error;
}

char *lexeme_for(char *symbuf, int token) {
    switch (token) {
        case ALT:    symbuf = "|"; break;
        case STAR:   symbuf = "*"; break;
        case LPAREN: symbuf = "("; break;
        case RPAREN: symbuf = ")"; break;
        case PRINTABLE:
            symbuf[0] = token;
            break;
    }

    return symbuf;
}

#define ERROR_FMT_STRING "| Parse Error:\n|\n| Got: %s\n| Expected: %s\n|\n| At Column: %d\n|\n\n"

void print_error(struct parse_error error) {
    char symbuf[2] = { 0, 0 };

    fprintf(stderr, ERROR_FMT_STRING,
        lexeme_for(symbuf, error.actual),
        lexeme_for(symbuf, error.expected),
        error.token_col);
}
