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

#define FIRST (enum regex_symbol[])
static enum regex_symbol const * const first_sets[] = {
    [EOF_T] =         FIRST { EOF_T, 0 },
    [SYMBOL_T] =      FIRST { SYMBOL_T, 0 },
    [RANGE_T] =       FIRST { RANGE_T, 0 },
    [NUM_T] =         FIRST { NUM_T, 0 },
    [ID_T] =          FIRST { ID_T, 0 },
    [ALT_T] =         FIRST { ALT_T, 0 },
    [STAR_T] =        FIRST { STAR_T, 0 },
    [PLUS_T] =        FIRST { PLUS_T, 0 },
    [OPTIONAL_T] =    FIRST { OPTIONAL_T, 0 },
    [DOTALL_T] =      FIRST { DOTALL_T, 0 },
    [LPAREN_T] =      FIRST { LPAREN_T, 0 },
    [RPAREN_T] =      FIRST { RPAREN_T, 0 },
    [CLASS_T] =       FIRST { CLASS_T, 0 },
    [NEG_CLASS_T] =   FIRST { NEG_CLASS_T, 0 },
    [END_CLASS_T] =   FIRST { END_CLASS_T, 0 },
    [ID_BRACE_T] =    FIRST { ID_BRACE_T, 0 },
    [LBRACE_T] =      FIRST { LBRACE_T, 0 },
    [RBRACE_T] =      FIRST { RBRACE_T, 0 },

    [REGEX_NT] =      FIRST { LPAREN_T, ID_BRACE_T, CLASS_T, NEG_CLASS_T, DOTALL_T, SYMBOL_T, ALT_T, EOF_T, 0 },
    [EXPR_NT] =       FIRST { LPAREN_T, ID_BRACE_T, CLASS_T, NEG_CLASS_T, DOTALL_T, SYMBOL_T, ALT_T, 0 },
    [ALT_NT] =        FIRST { LPAREN_T, ID_BRACE_T, CLASS_T, NEG_CLASS_T, DOTALL_T, SYMBOL_T, 0 },
    [ALTS_NT] =       FIRST { ALT_T, 0 },
    [FACTOR_NT] =     FIRST { LPAREN_T, ID_BRACE_T, CLASS_T, NEG_CLASS_T, DOTALL_T, SYMBOL_T, 0 },
    [FACTORS_NT] =    FIRST { LPAREN_T, ID_BRACE_T, CLASS_T, NEG_CLASS_T, DOTALL_T, SYMBOL_T, 0 },
    [CHAR_CLASS_NT] = FIRST { RANGE_T, END_CLASS_T, 0 },
    [RANGES_NT] =     FIRST { RANGE_T, 0 },
    [UNOPS_NT] =      FIRST { STAR_T, PLUS_T, OPTIONAL_T, LBRACE_T, 0 },
};
#undef FIRST

static enum regex_symbol const * const first_set(enum regex_symbol sym) {
    return first_sets[sym];
}

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
            case 'f': sym = '\f'; break;
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
        } else {
            range.end = range.start;
        }

        if (range.start <= range.end) {
            token.type = RANGE_T;
            token.val.range = range;
        } else {
            token.type = ERROR;
        }
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

                    if (*ip == '_' || isalpha(*ip)) {
                        type = ID_BRACE_T;
                    }
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

bool do_action(enum regex_symbol action, union regex_result val, struct parse_context *context) {
    pdebug("doing action: %s\n", str_for_sym(action));
    return (*context->actions[AI(action)])(val, context);
}

struct parse_context parse_context(
    void *result_context,
    union regex_result (*get_result)(void *result_context),
    bool (**actions)(union regex_result val, struct parse_context *context),
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
    enum regex_symbol const *s = first_set(expected);

    while (*s) {
        if (context->lookahead == *s) return true;
        else s++;
    }

    return false;
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

bool set_syntax_error(enum regex_symbol expected, struct parse_context *context) {
    if (!context->has_error) {
        pdebug("parse error %s\n", str_for_sym(expected));
        context->has_error = true;
        context->error = (struct parse_error) {
            .type = SYNTAX_ERROR,
            .actual = context->lookahead,
            .lexeme_col = context->lookahead_col,
            .expected = first_set(expected)
        };
    }

    return false;
}

bool set_oom_error(struct parse_context *context) {
    context->has_error = true;
    context->error = (struct parse_error) {
        .type = OUT_OF_MEMORY
    };

    return false;
}

static void print_symbol_list(FILE *handle, enum regex_symbol const *sym) {
    if (*sym) {
        fprintf(handle, "%s", str_for_sym(*sym));
        sym++;

        while (*sym) {
            fprintf(handle, ", %s", str_for_sym(*sym));
            sym++;
        }
    }
}

void print_parse_error(struct parse_error error) {
    switch (error.type) {
        case SYNTAX_ERROR:
            fprintf(stderr, SYNERR_FMT_STRING, str_for_sym(error.actual));
            print_symbol_list(stderr, error.expected);
            fprintf(stderr, SYNERR_FMT_STRING_END, error.lexeme_col);
            break;
        case OUT_OF_MEMORY:
            fprintf(stderr, OOM_FMT_STRING);
            break;
        case REGEX_NOT_DEFINED:
            // noop
            break;
    }
}

bool has_parse_error(struct parse_context *context) {
    return context->has_error;
}

struct parse_error nullperr() {
    return (struct parse_error) { 0, { } };
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
        case NUM_T:             return "num";
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
        case ID_BRACE_T:        return "{id";
        case LBRACE_T:          return "{";
        case RBRACE_T:          return "}";

        case REGEX_NT:          return "REGEX";
        case EXPR_NT:           return "EXPR";
        case ALT_NT:            return "ALT";
        case ALTS_NT:           return "ALTS";
        case FACTOR_NT:         return "FACTOR";
        case FACTORS_NT:        return "FACTORS";
        case CHAR_CLASS_NT:     return "CHAR_CLASS";
        case RANGES_NT:         return "RANGES";
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

