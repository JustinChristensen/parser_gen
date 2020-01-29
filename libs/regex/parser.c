#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <base/debug.h>
#include "regex/base.h"
#include "regex/parser.h"
#include "regex/result_types.h"

#define sdebug(...) debug_ns_("scanner", __VA_ARGS__)
#define pdebug(...) debug_ns_("parser", __VA_ARGS__)

struct regex_token regex_token(char *input) {
    return scan((struct regex_token) {
        .input = input,
        .input_col = 1,
        .in_class = false,
        .in_braces = false
    });
}

static struct regex_token consume(struct regex_token token, char c) {
    while (*token.input == c || isblank(*token.input)) {
        token.input++;
        token.input_col++;
    }

    return token;
}

static struct regex_token scan_escape(struct regex_token token) {
    char *ip = token.input;
    int ic = token.input_col;
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

    token.val.sym = sym;
    token.type = type;
    token.input = ++ip;
    token.input_col = ++ic;

    return token;
}

static struct regex_token scan_range(struct regex_token token) {
    token.type = ERROR;
    token = scan_escape(token);

    if (token.type == SYMBOL_T) {
        struct char_range range;

        range.start = token.val.sym;

        if (*token.input == '-') {
            token.input++;
            token.input_col++;

            token = scan_escape(token);
            range.end = token.val.sym;

            if (range.start > range.end) {
                char start = range.start;
                range.start = range.end;
                range.end = start;
            }
        } else {
            range.end = range.start;
        }

        token.val.range = range;
        token.type = RANGE_T;
    }

    return token;
}

struct regex_token scan(struct regex_token token) {
    char *ip = token.input;
    int ic = token.input_col;
    enum regex_symbol type = ERROR;

    token.lexeme = ip;
    token.lexeme_col = ic;

    sdebug("remaining: \"%s\"\n", ip);

    if (*ip == '\0') {
        type = EOF_T;
    } else if (token.in_class) {
        if (*ip == ']') {
            type = END_CLASS_T;
            token.in_class = false;
            ic++, ip++;
        } else if (*ip == '\\' || is_symbol(*ip)) {
            token = scan_range(token);
            ip = token.input;
            ic = token.input_col;
            type = token.type;
        } else {
            ic++, ip++;
        }
    } else if (token.in_braces) {
        if (*ip == '}') {
            type = RBRACE_T;
            token.in_braces = false;
            ic++, ip++;
        } else if (*ip == '_' || isalpha(*ip)) {
            type = ID_T;
            ic++, ip++;
            while (*ip == '_' || isalnum(*ip))
                ic++, ip++;
        } else if (isdigit(*ip)) {
            char *end;
            type = NUM_T;
            token.val.num = strtol(ip, &end, 10);
            ic += end - ip;
            ip = end;
        } else {
            ic++, ip++;
        }
    } else {
        if (*ip == '*') {
            token = consume(token, '*');
            type = STAR_T;
            ip = token.input, ic = token.input_col;
        } else if (*ip == '+') {
            token = consume(token, '+');
            type = PLUS_T;
            ip = token.input, ic = token.input_col;
        } else if (*ip == '?') {
            token = consume(token, '?');
            type = OPTIONAL_T;
            ip = token.input, ic = token.input_col;
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
                    token.in_braces = true;
                    ic++, ip++;
                    break;
                case '[':
                    type = CLASS_T;
                    token.in_class = true;
                    ic++, ip++;

                    if (*ip == '^') {
                        ic++, ip++;
                        type = NEG_CLASS_T;
                    }
                    break;
                default:
                    if (*ip == '\\' || is_symbol(*ip)) {
                        token = scan_escape(token);
                        type = token.type, ip = token.input, ic = token.input_col;
                    }
                    break;
            }
        }
    }

    token.input = ip;
    token.input_col = ic;
    token.type = type;

    sdebug("token: %s\n", str_for_sym(token.type));

    return token;
}

enum regex_symbol token_type(struct regex_token token) {
    return token.type;
}

union regex_token_val token_val(struct regex_token token) {
    return token.val;
}

int token_col(struct regex_token token) {
    return token.lexeme_col;
}

void token_lexeme(char *lbuf, struct regex_token token) {
    strncpy(lbuf, token.lexeme, token.input - token.lexeme);
}

static void print_token_val(struct regex_token token) {
    if (token.type == SYMBOL_T) {
        printf("%d ", token.val.sym);
    } else if (token.type == NUM_T) {
        printf("%d ", token.val.num);
    } else if (token.type == RANGE_T) {
        printf("%d %d ", token.val.range.start, token.val.range.end);
    }
}

void print_token(struct regex_token token) {
    char lexeme[BUFSIZ] = "";
    token_lexeme(lexeme, token);
    printf("%3d %-17s %-20s ", token_col(token), str_for_sym(token.type), lexeme);
    print_token_val(token);
    printf("\n");
}

void print_token_table(char *regex) {
    struct regex_token token = regex_token(regex);

    printf("%-3s %-17s %-20s %-s\n", "col", "symbol", "lexeme", "value");
    while (true) {
        print_token(token);
        if (token.type == EOF_T) break;
        else token = scan(token);
    }
}

union regex_result result(struct parse_context *context) {
    return (*context->get_result)(context->result_context);
}

void do_action(enum regex_symbol action, union regex_result val, struct parse_context *context) {
    pdebug("doing action: %s\n", str_for_sym(action));
    (*context->actions[AI(action)])(context->result_context, val);
}

struct parse_context parse_context(
    void *result_context,
    union regex_result (*get_result)(void *result_context),
    void (**actions)(void *result_context, union regex_result lval),
    bool use_nonrec
) {
    assert(result_context != NULL);
    assert(actions != NULL);
    assert(get_result != NULL);

    struct parse_context context = {
        .result_context = result_context,
        .actions = actions,
        .get_result = get_result,
        .has_error = false,
        .error = nullperr(),
        .use_nonrec = use_nonrec
    };

    return context;
}

void start_scanning(char *input, struct parse_context *context) {
    struct regex_token token = regex_token(input);
    context->token = token;
    context->lookahead = token_type(token);
    context->lookahead_col = token_col(token);
    context->lookahead_val = token_val(token);
    context->has_error = false;
}

bool peek(enum regex_symbol expected, struct parse_context *context) {
    return context->lookahead == expected;
}

bool expect(enum regex_symbol expected, struct parse_context *context) {
    if (peek(expected, context)) {
        pdebug("success, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(context->lookahead));

        struct regex_token token = scan(context->token);

        context->token = token;
        context->lookahead = token_type(token);
        context->lookahead_col = token_col(token);
        context->lookahead_val = token_val(token);

        return true;
    }

    pdebug("failure, expected \"%s\", actual \"%s\"\n",
        str_for_sym(expected), str_for_sym(context->lookahead));

    set_parse_error(expected, context);

    return false;
}

int is_symbol(int c) {
    return isprint(c);
}

enum regex_symbol lookahead(struct parse_context *context) {
    return context->lookahead;
}

union regex_result lookahead_val(struct parse_context *context) {
    return (union regex_result) { .tval = context->lookahead_val };
}

union regex_result id_val(char *idbuf, struct parse_context *context) {
    token_lexeme(idbuf, context->token);
    return (union regex_result) { .id = idbuf };
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
        case ERROR:             return "ERROR";

        case EOF_T:             return "eof";
        case SYMBOL_T:          return "a";
        case RANGE_T:           return "a-z";
        case NUM_T:             return "999";
        case ID_T:              return "id";
        case ALT_T:             return "|";
        case STAR_T:            return "*";
        case PLUS_T:            return "+";
        case OPTIONAL_T:        return "?";
        case DOTALL_T:          return ".";
        case LPAREN_T:          return "(";
        case RPAREN_T:          return ")";
        case CLASS_T:           return "[";
        case NEG_CLASS_T:       return "[^";
        case END_CLASS_T:       return "]";
        case LBRACE_T:          return "{";
        case RBRACE_T:          return "}";

        case REGEX_NT:          return "REGEX";
        case ALTS_HEAD_NT:      return "ALTS_HEAD";
        case ALTS_NT:           return "ALTS";
        case ALT_NT:            return "ALT";
        case FACTORS_NT:        return "FACTORS";
        case RANGES_NT:         return "RANGES";
        case FACTOR_NT:         return "FACTORS";
        case UNOPS_NT:          return "UNOPS";

        case DO_REGEX:          return "{regex}";
        case DO_EMPTY:          return "{empty}";
        case DO_ALT:            return "{alt}";
        case DO_CAT:            return "{cat}";
        case DO_SUB:            return "{sub}";
        case DO_ID:             return "{id}";
        case DO_CHAR_CLASS:     return "{char_class}";
        case DO_NEG_CLASS:      return "{neg_class}";
        case DO_DOTALL:         return "{dotall}";
        case DO_SYMBOL:         return "{symbol}";
        case DO_RANGE:          return "{range}";
        case DO_STAR:           return "{star}";
        case DO_PLUS:           return "{plus}";
        case DO_OPTIONAL:       return "{optional}";
        case DO_REPEAT_EXACT:   return "{repeat_exact}";
    }
}

void print_parse_error(struct parse_error error) {
    fprintf(stderr, ERROR_FMT_STRING,
        str_for_sym(error.actual),
        str_for_sym(error.expected),
        error.lexeme_col);
}
