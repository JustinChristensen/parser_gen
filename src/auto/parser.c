#include <stdlib.h>
#include <stdio.h>
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

#ifdef DEBUG
    printf("%s\n", context.input);
#endif

    switch (*context.input) {
        case '*':
            context.token = STAR;
            break;
        case '|':
            context.token = ALT;
            break;
        case '(':
            context.token = LPAREN;
            break;
        case ')':
            context.token = RPAREN;
            break;
        default:
            if (*context.input == '\\') {
                context.input++;
                context.input_col++;
            }

            context.token = *context.input;
            break;
    }

    context.input++;
    context.input_col++;

    return context;
}

int token(struct scan_context context) {
    return context.token;
}

int token_col(struct scan_context context) {
    return context.token_col;
}

struct parse_context parse_context(char *input, struct expr *exprbuf) {
    struct scan_context scontext = scan(scan_context(input));

    struct parse_context context = {
        .exprbuf = exprbuf,
        .scan_context = scontext,
        .lookahead = token(scontext),
        .lookahead_col = token_col(scontext),
        .has_error = false,
        .expr = NULL,
        .error = (struct parse_error) { 0, 0, 0 }
    };

    sexpr(&context, empty_expr());

    return context;
}

bool peek(struct parse_context *context, int expected, int (*is) (int c)) {
    return (is && (*is)(context->lookahead)) || context->lookahead == expected;
}

bool expect(struct parse_context *context, int expected, int (*is) (int c)) {
    if (peek(context, expected, is)) {
#ifdef DEBUG
        char symbuf[2] = { 0, 0 };
        printf("success: expected \"%s\", actual \"%s\"\n",
            lexeme_for(symbuf, expected), lexeme_for(symbuf, context->lookahead));
#endif
        struct scan_context scan_context = scan(context->scan_context);
        context->scan_context = scan_context;
        context->lookahead = token(scan_context);
        context->lookahead_col = token_col(scan_context);

        return true;
    } else {
#ifdef DEBUG
        char symbuf[2] = { 0, 0 };
        printf("failure: expected \"%s\", actual \"%s\"\n",
            lexeme_for(symbuf, expected), lexeme_for(symbuf, context->lookahead));
#endif
        context->has_error = true;
        context->error = (struct parse_error) {
            .actual = context->lookahead,
            .token_col = context->lookahead_col,
            .expected = expected
        };

        return false;
    }
}

int is_symbol(int c) {
    switch (c + OPERATOR_OFFSET) {
        case ALT:
        case STAR:
        case LPAREN:
        case RPAREN:
            return false;
    }
    return isprint(c);
}

int lookahead(struct parse_context *context) {
    return context->lookahead;
}

bool parse_expr(struct parse_context *context) {
    bool success = true;

    if (peek(context, '\0', NULL)) return success;

    if (peek(context, LPAREN, NULL)) {
        expect(context, LPAREN, NULL);
        if (parse_expr(context) && expect(context, RPAREN, NULL)) {
            sexpr(context, sub_expr(gexpr(context)));
        } else {
            success = false;
        }
    } else if (peek(context, SYMBOL, is_symbol)) {
        int sym = lookahead(context);
        expect(context, SYMBOL, is_symbol);
        sexpr(context, symbol_expr((char) sym));
    }

    while (!peek(context, '\0', NULL)) {
        struct expr *lexpr = gexpr(context);

        if (peek(context, ALT, NULL)) {
            expect(context, ALT, NULL);
            if (parse_expr(context)) {
                sexpr(context, alt_expr(lexpr, gexpr(context)));
                continue;
            }
        } else if (parse_expr(context)) {
            sexpr(context, cat_expr(lexpr, gexpr(context)));
            continue;
        } else if (peek(context, STAR, NULL)) {
            expect(context, STAR, NULL);
            sexpr(context, star_expr(lexpr));
            continue;
        }

        success = false;
        break;
    }

    return success;
}

void sexpr(struct parse_context *context, struct expr expr) {
    *context->exprbuf = expr;
    context->expr = context->exprbuf;
    context->exprbuf++;
}

struct expr *gexpr(struct parse_context *context) {
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
        case ALT:       symbuf = "|"; break;
        case STAR:      symbuf = "*"; break;
        case LPAREN:    symbuf = "("; break;
        case RPAREN:    symbuf = ")"; break;
        case SYMBOL:    symbuf = "symbol"; break;
        default:
            symbuf[0] = token;
            break;
    }

    return symbuf;
}

#define ERROR_FMT_STRING "| Parse Error\n|\n| Got: %s\n| Expected: %s\n|\n| At Column: %d\n|\n"

void print_error(struct parse_error error) {
    char symbuf[2] = { 0, 0 };

    fprintf(stderr, ERROR_FMT_STRING,
        lexeme_for(symbuf, error.actual),
        lexeme_for(symbuf, error.expected),
        error.token_col);
}
