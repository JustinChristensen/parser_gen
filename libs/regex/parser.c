#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <base/debug.h>
#include "regex/base.h"
#include "parser.h"

static struct regex_token scan_range(struct regex_token token);
static void print_token_val(struct regex_token token);
static union regex_result get_result(struct regex_parse_context *context);
static bool result_has_error(struct regex_parse_context *context);
static struct regex_error result_error(struct regex_parse_context *context);
static bool do_action(enum regex_symbol action, union regex_result val, struct regex_parse_context *context);
static void start_scanning(char *input, struct regex_parse_context *context);
static bool peek(enum regex_symbol expected, struct regex_parse_context *context);
static bool expect(enum regex_symbol expected, struct regex_parse_context *context);
static enum regex_symbol lookahead(struct regex_parse_context *context);
static union regex_result lookahead_val(struct regex_parse_context *context);
static union regex_result tag_val(char *tag, struct regex_parse_context *context);
static bool set_syntax_error(enum regex_symbol expected, struct regex_parse_context *context);
static bool parse_regex_rec(char *pattern, struct regex_parse_context *context);
static bool parse_regex_nonrec(char *pattern, struct regex_parse_context *context);

#define sdebug(...) debug_ns_("regex_scanner", __VA_ARGS__)
#define pdebug(...) debug_ns_("regex_parser", __VA_ARGS__)

#define FIRST (enum regex_symbol[])
static enum regex_symbol const * const first_sets[] = {
    [RX_EOF_T] =         FIRST { RX_EOF_T, 0 },
    [RX_CHAR_T] =        FIRST { RX_CHAR_T, 0 },
    [RX_RANGE_T] =       FIRST { RX_RANGE_T, 0 },
    [RX_NUM_T] =         FIRST { RX_NUM_T, 0 },
    [RX_TAG_T] =         FIRST { RX_TAG_T, 0 },
    [RX_ALT_T] =         FIRST { RX_ALT_T, 0 },
    [RX_STAR_T] =        FIRST { RX_STAR_T, 0 },
    [RX_PLUS_T] =        FIRST { RX_PLUS_T, 0 },
    [RX_OPTIONAL_T] =    FIRST { RX_OPTIONAL_T, 0 },
    [RX_DOTALL_T] =      FIRST { RX_DOTALL_T, 0 },
    [RX_LPAREN_T] =      FIRST { RX_LPAREN_T, 0 },
    [RX_RPAREN_T] =      FIRST { RX_RPAREN_T, 0 },
    [RX_CLASS_T] =       FIRST { RX_CLASS_T, 0 },
    [RX_NEG_CLASS_T] =   FIRST { RX_NEG_CLASS_T, 0 },
    [RX_END_CLASS_T] =   FIRST { RX_END_CLASS_T, 0 },
    [RX_TAG_BRACE_T] =   FIRST { RX_TAG_BRACE_T, 0 },
    [RX_LBRACE_T] =      FIRST { RX_LBRACE_T, 0 },
    [RX_RBRACE_T] =      FIRST { RX_RBRACE_T, 0 },

    [RX_REGEX_NT] =      FIRST { RX_LPAREN_T, RX_TAG_BRACE_T, RX_CLASS_T, RX_NEG_CLASS_T, RX_DOTALL_T, RX_CHAR_T, RX_ALT_T, RX_EOF_T, 0 },
    [RX_EXPR_NT] =       FIRST { RX_LPAREN_T, RX_TAG_BRACE_T, RX_CLASS_T, RX_NEG_CLASS_T, RX_DOTALL_T, RX_CHAR_T, RX_ALT_T, 0 },
    [RX_ALT_NT] =        FIRST { RX_LPAREN_T, RX_TAG_BRACE_T, RX_CLASS_T, RX_NEG_CLASS_T, RX_DOTALL_T, RX_CHAR_T, 0 },
    [RX_ALTS_NT] =       FIRST { RX_ALT_T, 0 },
    [RX_FACTOR_NT] =     FIRST { RX_LPAREN_T, RX_TAG_BRACE_T, RX_CLASS_T, RX_NEG_CLASS_T, RX_DOTALL_T, RX_CHAR_T, 0 },
    [RX_FACTORS_NT] =    FIRST { RX_LPAREN_T, RX_TAG_BRACE_T, RX_CLASS_T, RX_NEG_CLASS_T, RX_DOTALL_T, RX_CHAR_T, 0 },
    [RX_CHAR_CLASS_NT] = FIRST { RX_RANGE_T, RX_END_CLASS_T, 0 },
    [RX_RANGES_NT] =     FIRST { RX_RANGE_T, 0 },
    [RX_UNOPS_NT] =      FIRST { RX_STAR_T, RX_PLUS_T, RX_OPTIONAL_T, RX_LBRACE_T, 0 },
};
#undef FIRST

static enum regex_symbol const * const first_set(enum regex_symbol sym) {
    return first_sets[sym];
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
    enum regex_symbol type = RX_ERROR;
    char ch = '\0';

    if (*ip == '\\') {
        ip++, ic++;

        switch (*ip) {
            case 'f': ch = '\f'; break;
            case 'n': ch = '\n'; break;
            case 'r': ch = '\r'; break;
            case 't': ch = '\t'; break;
            case 'v': ch = '\v'; break;
            default: break;
        }
    }

    type = RX_CHAR_T;
    if (!ch) ch = *ip;
    ip++, ic++;

    token.val.ch = ch;
    token.type = type;
    token.input = ip;
    token.input_col = ic;

    return token;
}

static struct regex_token scan_range(struct regex_token token) {
    token.type = RX_ERROR;
    token = scan_escape(token);

    if (token.type == RX_CHAR_T) {
        struct regex_char_range range;

        range.start = token.val.ch;

        if (*token.input == '-') {
            token.input++;
            token.input_col++;

            token = scan_escape(token);
            range.end = token.val.ch;
        } else {
            range.end = range.start;
        }

        if (range.start <= range.end) {
            token.type = RX_RANGE_T;
            token.val.range = range;
        } else {
            token.type = RX_ERROR;
        }
    }

    return token;
}

struct regex_token regex_token(char *pattern) {
    return regex_scan((struct regex_token) {
        .input = pattern,
        .input_col = 1,
        .in_class = false,
        .in_braces = false
    });
}

struct regex_token regex_scan(struct regex_token token) {
    char *ip = token.input;
    int ic = token.input_col;
    enum regex_symbol type = RX_ERROR;

    token.lexeme = ip;
    token.lexeme_col = ic;

    sdebug("remaining: \"%s\"\n", ip);

    if (*ip == '\0') {
        type = RX_EOF_T;
    } else if (token.in_class) {
        if (*ip == ']') {
            type = RX_END_CLASS_T;
            token.in_class = false;
            ic++, ip++;
        } else {
            token = scan_range(token);
            ip = token.input;
            ic = token.input_col;
            type = token.type;
        }
    } else if (token.in_braces) {
        if (*ip == '}') {
            type = RX_RBRACE_T;
            token.in_braces = false;
            ic++, ip++;
        } else if (*ip == '_' || isalpha(*ip)) {
            type = RX_TAG_T;
            ic++, ip++;
            while (*ip == '_' || isalnum(*ip))
                ic++, ip++;
        } else if (isdigit(*ip)) {
            char *end;
            type = RX_NUM_T;
            token.val.num = strtol(ip, &end, 10);
            ic += end - ip;
            ip = end;
        } else {
            ic++, ip++;
        }
    } else {
        if (*ip == '*') {
            token = consume(token, '*');
            type = RX_STAR_T;
            ip = token.input, ic = token.input_col;
        } else if (*ip == '+') {
            token = consume(token, '+');
            type = RX_PLUS_T;
            ip = token.input, ic = token.input_col;
        } else if (*ip == '?') {
            token = consume(token, '?');
            type = RX_OPTIONAL_T;
            ip = token.input, ic = token.input_col;
        } else {
            switch (*ip) {
                case '.':
                    type = RX_DOTALL_T;
                    ic++, ip++;
                    break;
                case '|':
                    type = RX_ALT_T;
                    ic++, ip++;
                    break;
                case '(':
                    type = RX_LPAREN_T;
                    ic++, ip++;
                    break;
                case ')':
                    type = RX_RPAREN_T;
                    ic++, ip++;
                    break;
                case '{':
                    type = RX_LBRACE_T;
                    token.in_braces = true;
                    ic++, ip++;

                    if (*ip == '_' || isalpha(*ip)) {
                        type = RX_TAG_BRACE_T;
                    }
                    break;
                case '[':
                    type = RX_CLASS_T;
                    token.in_class = true;
                    ic++, ip++;

                    if (*ip == '^') {
                        ic++, ip++;
                        type = RX_NEG_CLASS_T;
                    }
                    break;
                default:
                    token = scan_escape(token);
                    type = token.type, ip = token.input, ic = token.input_col;
                    break;
            }
        }
    }

    token.input = ip;
    token.input_col = ic;
    token.type = type;

    sdebug("token: %s\n", str_for_regex_sym(token.type));

    return token;
}

enum regex_symbol regex_token_type(struct regex_token token) {
    return token.type;
}

union regex_token_val regex_token_val(struct regex_token token) {
    return token.val;
}

int regex_token_col(struct regex_token token) {
    return token.lexeme_col;
}

void regex_token_lexeme(char *lexeme, struct regex_token token) {
    size_t lsize = token.input - token.lexeme;
    strncpy(lexeme, token.lexeme, lsize);
    lexeme[lsize] = '\0';
}

static void print_token_val(struct regex_token token) {
    if (token.type == RX_CHAR_T) {
        printf("%d ", token.val.ch);
    } else if (token.type == RX_NUM_T) {
        printf("%d ", token.val.num);
    } else if (token.type == RX_RANGE_T) {
        printf("%d %d ", token.val.range.start, token.val.range.end);
    }
}

void print_regex_token(struct regex_token token) {
    char lexeme[BUFSIZ] = "";
    regex_token_lexeme(lexeme, token);
    printf("%3d %-17s %-20s ", regex_token_col(token), str_for_regex_sym(token.type), lexeme);
    print_token_val(token);
    printf("\n");
}

void print_regex_token_table(char *regex) {
    struct regex_token token = regex_token(regex);

    printf("%-3s %-17s %-20s %-s\n", "col", "symbol", "lexeme", "value");
    while (true) {
        print_regex_token(token);
        if (token.type == RX_EOF_T) break;
        else token = regex_scan(token);
    }
}

static union regex_result get_result(struct regex_parse_context *context) {
    return (*context->pi.result)(context->result);
}

static bool result_has_error(struct regex_parse_context *context) {
    return context->pi.has_error ? (*context->pi.has_error)(context->result) : false;
}

static struct regex_error result_error(struct regex_parse_context *context) {
    return context->pi.error ? (*context->pi.error)(context->result) : regex_nullerror();
}

static bool do_action(enum regex_symbol action, union regex_result val, struct regex_parse_context *context) {
    pdebug("doing action: %s\n", str_for_regex_sym(action));

    if (!(*context->pi.actions[AI(action)])(val, context->result)) {
        context->has_error = result_has_error(context);
        context->error = result_error(context);
        return false;
    }

    return true;
}

struct regex_parse_context regex_parse_context(
    void *result,
    struct regex_parse_interface pi
) {
    assert(result != NULL);

    struct regex_parse_context context = {
        .result = result,
        .pi = pi,
        .has_error = false,
        .error = regex_nullerror()
    };

    return context;
}

bool parse_regex(char *pattern, struct regex_parse_context *context) {
    if (getenv("USE_NONREC")) {
        return parse_regex_nonrec(pattern, context);
    } else {
        return parse_regex_rec(pattern, context);
    }
}

static void start_scanning(char *input, struct regex_parse_context *context) {
    struct regex_token token = regex_token(input);
    context->token = token;
    context->lookahead = regex_token_type(token);
    context->lookahead_col = regex_token_col(token);
    context->lookahead_val = regex_token_val(token);
    context->has_error = false;
}

static bool peek(enum regex_symbol expected, struct regex_parse_context *context) {
    enum regex_symbol const *s = first_set(expected);

    while (*s) {
        if (context->lookahead == *s) return true;
        else s++;
    }

    return false;
}

static bool expect(enum regex_symbol expected, struct regex_parse_context *context) {
    if (peek(expected, context)) {
        pdebug("success, expected \"%s\", actual \"%s\"\n",
            str_for_regex_sym(expected), str_for_regex_sym(context->lookahead));

        struct regex_token token = regex_scan(context->token);

        context->token = token;
        context->lookahead = regex_token_type(token);
        context->lookahead_col = regex_token_col(token);
        context->lookahead_val = regex_token_val(token);

        return true;
    }

    pdebug("failure, expected \"%s\", actual \"%s\"\n",
        str_for_regex_sym(expected), str_for_regex_sym(context->lookahead));

    return false;
}

static enum regex_symbol lookahead(struct regex_parse_context *context) {
    return context->lookahead;
}

static union regex_result lookahead_val(struct regex_parse_context *context) {
    return (union regex_result) { .tval = context->lookahead_val };
}

static union regex_result tag_val(char *tagbuf, struct regex_parse_context *context) {
    regex_token_lexeme(tagbuf, context->token);
    return (union regex_result) { .tag = tagbuf };
}

static bool set_syntax_error(enum regex_symbol expected, struct regex_parse_context *context) {
    if (!context->has_error) {
        pdebug("parse error %s\n", str_for_regex_sym(expected));
        context->has_error = true;
        context->error = regex_syntax_error(context->lookahead, context->lookahead_col, first_set(expected));
    }

    return false;
}

bool has_parse_error(struct regex_parse_context *context) {
    return context->has_error;
}

struct regex_error regex_parse_error(struct regex_parse_context *context) {
    return context->error;
}

#include "parser/rec.c"
#include "parser/nonrec.c"
