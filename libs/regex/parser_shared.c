#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>
#include <base/debug.h>
#include "regex/parser.h"
#include "regex/result_types.h"

#define sdebug(...) debug_ns_("scanner", __VA_ARGS__)
#define pdebug(...) debug_ns_("parser", __VA_ARGS__)

struct scan_context scan_context(char *input) {
    return (struct scan_context) {
        .input = input,
        .input_col = 1,
        .token_col = 1
    };
}

struct scan_context consume(struct scan_context context, char c) {
    while (*context.input == c || isblank(*context.input)) {
        context.input++;
        context.input_col++;
    }

    return context;
}

struct scan_context scan(struct scan_context context) {
    while (isblank(*context.input)) {
        context.input_col++;
        context.input++;
    }

    context.token_col = context.input_col;

    sdebug("remaining \"%s\"\n", context.input);

    if (*context.input == '*') {
        context = consume(context, '*');
        context.token = STAR;
    } else if (*context.input == '+') {
        context = consume(context, '+');
        context.token = PLUS;
    } else if (*context.input == '?') {
        context = consume(context, '?');
        context.token = OPTIONAL;
    } else if (*context.input == '\0') {
        context.token = EOI;
    } else {
        switch (*context.input) {
            case '.':
                context.token = DOTALL;
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

                context.token = NONSYM;

                if (is_symbol(*context.input)) {
                    context.token = SYMBOL;
                    context.symbol = *context.input;
                }
                break;
        }

        context.input++;
        context.input_col++;
    }

    return context;
}

int token(struct scan_context context) {
    return context.token;
}

char token_sym(struct scan_context context) {
    return context.symbol;
}

int token_col(struct scan_context context) {
    return context.token_col;
}

union rval getval(struct parse_context *context) {
    return (*context->getval)(context->result_context);
}

void do_action(struct parse_context *context, enum action_type action, union rval lval) {
    (*context->actions[action])(context->result_context, lval);
}

struct parse_context parse_context(
    char *input,
    void *result_context,
    union rval (*getval)(void *result_context),
    void (**actions)(void *result_context, union rval lval),
    bool use_nonrec
) {
    assert(input != NULL);
    assert(result_context != NULL);
    assert(actions != NULL);
    assert(getval != NULL);

    struct scan_context scontext = scan(scan_context(input));

    struct parse_context context = {
        .scan_context = scontext,
        .result_context = result_context,
        .actions = actions,
        .getval = getval,
        .lookahead = token(scontext),
        .lookahead_col = token_col(scontext),
        .symbol = token_sym(scontext),
        .has_error = false,
        .error = nullperr(),
        .use_nonrec = use_nonrec
    };

    return context;
}

bool peek(struct parse_context *context, int expected) {
    return context->lookahead == expected;
}

bool expect(struct parse_context *context, int expected) {
    if (peek(context, expected)) {
        pdebug("success, expected \"%s\", actual \"%s\"\n",
            lexeme_for(expected), lexeme_for(context->lookahead));

        struct scan_context scan_context = scan(context->scan_context);

        context->scan_context = scan_context;
        context->lookahead = token(scan_context);
        context->lookahead_col = token_col(scan_context);
        context->symbol = token_sym(scan_context);

        return true;
    } else {
        pdebug("failure, expected \"%s\", actual \"%s\"\n",
            lexeme_for(expected), lexeme_for(context->lookahead));
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
    return isprint(c);
}

char symbol(struct parse_context *context) {
    return context->symbol;
}

int lookahead(struct parse_context *context) {
    return context->lookahead;
}

bool has_parse_error(struct parse_context *context) {
    return context->has_error;
}

struct parse_error nullperr() {
    return (struct parse_error) { 0, 0, 0 };
}

struct parse_error parse_error(struct parse_context *context) {
    return context->error;
}

char *lexeme_for(int token) {
    switch (token) {
        case EOI:      return "eof";
        case ALT:      return "|";
        case STAR:     return "*";
        case PLUS:     return "+";
        case OPTIONAL: return "?";
        case DOTALL:   return ".";
        case LPAREN:   return "(";
        case RPAREN:   return ")";
        case SYMBOL:   return "symbol";
        case NONSYM:   return "newline or control character";
    }

    return "";
}

void print_parse_error(struct parse_error error) {
    fprintf(stderr, ERROR_FMT_STRING,
        lexeme_for(error.actual),
        lexeme_for(error.expected),
        error.token_col);
}
