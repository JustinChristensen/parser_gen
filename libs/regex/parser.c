#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
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
        .lexeme_col = 1
    };
}

static struct scan_context consume(struct scan_context context, char c) {
    while (*context.input == c || isblank(*context.input)) {
        context.input++;
        context.input_col++;
    }

    return context;
}

static struct scan_context scan_escape(struct scan_context context) {
    char *ip = context.input;
    int ic = context.input_col;
    enum regex_symbol type = ERROR;
    char sym = '\0';

    if (*ip == '\\') {
        ip++, ic++;

        switch (*ip) {
            case 'f': sym = '\n'; break;
            case 'n': sym = '\n'; break;
            case 'r': sym = '\r'; break;
            case 't': sym = '\t'; break;
            case 'v': sym = '\v'; break;
            default: break;
        }
    }

    if (sym) {
        type = SYMBOL_T;
    } else if (is_symbol(*ip)) {
        type = SYMBOL_T;
        sym = *ip;
    }

    context.sym = sym;
    context.type = type;
    context.input = ++ip;
    context.input_col = ++ic;

    return context;
}

static struct scan_context scan_range(struct scan_context context) {
    context.type = ERROR;
    context = scan_escape(context);

    if (context.type == SYMBOL_T) {
        char start = context.sym;

        if (*context.input == '-') {
            context.input++;
            context.input_col++;

            context = scan_escape(context);
            context.end = context.sym;
            context.start = start;

            if (context.start > context.end) {
                start = context.start;
                context.start = context.end;
                context.end = start;
            }
        } else {
            context.end = context.start;
        }

        context.type = RANGE_T;
    }

    return context;
}

struct scan_context scan(struct scan_context context) {
    char *ip = context.input;
    int ic = context.input_col;
    enum regex_symbol type = ERROR;

    context.lexeme = ip;
    context.lexeme_col = ic;

    sdebug("remaining: \"%s\"\n", ip);

    if (*ip == '\0') {
        type = EOF_T;
    } else if (context.in_class) {
        if (*ip == ']') {
            type = END_CLASS_T;
            context.in_class = false;
            ic++, ip++;
        } else if (*ip == '\\' || is_symbol(*ip)) {
            context = scan_range(context);
            ip = context.input;
            ic = context.input_col;
            type = context.type;
        } else {
            ic++, ip++;
        }
    } else if (context.in_braces) {
        if (*ip == '}') {
            type = RBRACE_T;
            context.in_braces = false;
            ic++, ip++;
        } else if (isdigit(*ip)) {
            char *end;
            type = NUM_T;
            context.num = strtol(ip, &end, 10);
            ic += end - ip;
            ip = end;
        } else {
            ic++, ip++;
        }
    } else {
        if (*ip == '*') {
            context = consume(context, '*');
            type = STAR_T;
            ip = context.input, ic = context.input_col;
        } else if (*ip == '+') {
            context = consume(context, '+');
            type = PLUS_T;
            ip = context.input, ic = context.input_col;
        } else if (*ip == '?') {
            context = consume(context, '?');
            type = OPTIONAL_T;
            ip = context.input, ic = context.input_col;
        } else {
            switch (*ip) {
                case '.':
                    type = DOTALL_T;
                    ic++, ip++;
                    break;
                case '|':
                    type = ALT_T;
                    ic++, ip++;
                    break;
                case '(':
                    type = LPAREN_T;
                    ic++, ip++;
                    break;
                case ')':
                    type = RPAREN_T;
                    ic++, ip++;
                    break;
                case '{':
                    type = LBRACE_T;
                    context.in_braces = true;
                    ic++, ip++;
                    break;
                case '[':
                    type = CLASS_T;
                    context.in_class = true;
                    ic++, ip++;

                    if (*ip == '^') {
                        ic++, ip++;
                        type = NEG_CLASS_T;
                    }
                    break;
                default:
                    if (*ip == '\\' || is_symbol(*ip)) {
                        context = scan_escape(context);
                        type = context.type, ip = context.input, ic = context.input_col;
                    }
                    break;
            }
        }
    }

    context.input = ip;
    context.input_col = ic;
    context.type = type;

    sdebug("token: %s\n", str_for_sym(context.type));

    return context;
}

enum regex_symbol token_type(struct scan_context context) {
    return context.type;
}

int lexeme_col(struct scan_context context) {
    return context.lexeme_col;
}

char *lexeme(struct scan_context context) {
    char *lexeme = strndup(context.lexeme, context.input - context.lexeme);
    assert(lexeme != NULL);
    return lexeme;
}

union rval getval(struct parse_context *context) {
    return (*context->getval)(context->result_context);
}

void do_action(struct parse_context *context, enum regex_symbol action, union rval lval) {
    pdebug("doing action: %s\n", str_for_sym(action));
    (*context->actions[AI(action)])(context->result_context, lval);
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
        .lookahead = token_type(scontext),
        .lookahead_col = lexeme_col(scontext),
        .symbol = scontext.sym,
        .has_error = false,
        .error = nullperr(),
        .use_nonrec = use_nonrec
    };

    return context;
}

bool peek(struct parse_context *context, enum regex_symbol expected) {
    return context->lookahead == expected;
}

bool expect(struct parse_context *context, enum regex_symbol expected) {
    if (peek(context, expected)) {
        pdebug("success, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(context->lookahead));

        struct scan_context scan_context = scan(context->scan_context);

        context->scan_context = scan_context;
        context->lookahead = token_type(scan_context);
        context->lookahead_col = lexeme_col(scan_context);
        context->symbol = scan_context.sym;

        return true;
    } else {
        pdebug("failure, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(context->lookahead));
        set_parse_error(expected, context);

        return false;
    }
}

int is_symbol(int c) {
    return isprint(c);
}

char symbol(struct parse_context *context) {
    return context->symbol;
}

enum regex_symbol lookahead(struct parse_context *context) {
    return context->lookahead;
}

void set_parse_error(enum regex_symbol expected, struct parse_context *context) {
    context->has_error = true;
    context->error = (struct parse_error) {
        .actual = context->lookahead,
        .lexeme_col = context->lookahead_col,
        .expected = expected
    };
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

char *str_for_sym(enum regex_symbol type) {
    switch (type) {
        case ERROR:            return "ERROR";

        case EOF_T:            return "EOF_T";
        case SYMBOL_T:         return "SYMBOL_T";
        case RANGE_T:          return "RANGE_T";
        case NUM_T:            return "NUM_T";
        case ALT_T:            return "ALT_T";
        case STAR_T:           return "STAR_T";
        case PLUS_T:           return "PLUS_T";
        case OPTIONAL_T:       return "OPTIONAL_T";
        case DOTALL_T:         return "DOTALL_T";
        case LPAREN_T:         return "LPAREN_T";
        case RPAREN_T:         return "RPAREN_T";
        case CLASS_T:          return "CLASS_T";
        case NEG_CLASS_T:      return "NEG_CLASS_T";
        case END_CLASS_T:      return "END_CLASS_T";
        case LBRACE_T:         return "LBRACE_T";
        case RBRACE_T:         return "RBRACE_T";

        case REGEX_NT:       return "REGEX";
        case EXPR_NT:        return "EXPR";
        case ALT_NT:         return "ALT";
        case ALT_TAIL_NT:    return "ALT_TAIL";
        case CAT_NT:         return "CAT";
        case CAT_TAIL_NT:    return "CAT_TAIL";
        case FACTOR_NT:      return "FACTOR";
        case FACTOR_TAIL_NT: return "FACTOR_TAIL";

        case DO_REGEX:       return "{regex}";
        case DO_EMPTY:       return "{empty}";
        case DO_ALT:         return "{alt}";
        case DO_CAT:         return "{cat}";
        case DO_SUB:         return "{sub}";
        case DO_DOTALL:      return "{dotall}";
        case DO_SYMBOL:      return "{symbol}";
        case DO_STAR:        return "{star}";
        case DO_PLUS:        return "{plus}";
        case DO_OPTIONAL:    return "{optional}";
    }
}

void print_parse_error(struct parse_error error) {
    fprintf(stderr, ERROR_FMT_STRING,
        str_for_sym(error.actual),
        str_for_sym(error.expected),
        error.lexeme_col);
}
