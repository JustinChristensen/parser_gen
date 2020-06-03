#include <stdlib.h>
#include <stdio.h>
#include "regex/base.h"

struct regex_error regex_syntax_error(
    enum regex_symbol const actual,
    struct regex_loc lexeme_loc,
    enum regex_symbol const *expected
) {
    return (struct regex_error) {
        .type = RX_SYNTAX_ERROR,
        .actual = actual,
        .lexeme_loc = lexeme_loc,
        .expected = expected
    };
}

struct regex_error regex_oom_error() {
    return (struct regex_error) { .type = RX_OUT_OF_MEMORY };
}

struct regex_error regex_repeat_zero_error() {
    return (struct regex_error) { .type = RX_REPEAT_ZERO };
}

struct regex_error regex_missing_tag_error(char *tag) {
    return (struct regex_error) {
        .type = RX_MISSING_TAG,
        .tag = tag
    };
}

struct regex_error regex_tag_exists_error(char *tag) {
    return (struct regex_error) {
        .type = RX_TAG_EXISTS,
        .tag = tag
    };
}

struct regex_error regex_duplicate_pattern_error(char *pattern) {
    return (struct regex_error) {
        .type = RX_DUPLICATE_PATTERN,
        .pattern = pattern
    };
}

struct regex_error regex_nullerror() {
    return (struct regex_error) { 0 };
}

void regex_escape(char *pattern) {
    char *c = pattern;

    while (*c) {
        char *n; char s; char t;

        switch (*c) {
            case '*':
            case '+':
            case '?':
            case '.':
            case '|':
            case '(':
            case ')':
            case '{':
            case '[':
                s = '\\';
                for (n = c; s; n++) {
                    t = *n;
                    *n = s;
                    s = t;
                }
                c++;
                break;
            default: break;
        }
        c++;
    }
}

struct regex_loc bump_regex_loc(char c, struct regex_loc loc) {
    if (c == '\n') {
        loc.line++;
        loc.col = 1;
    } else {
        loc.col++;
    }

    return loc;
}

struct regex_loc regex_loc(char *path, int line, int col) {
    return (struct regex_loc) { path, line, col };
}

struct regex_pattern regex_loc_pattern(int sym, char *tag, char *pattern, struct regex_loc loc) {
    return (struct regex_pattern) { sym, tag, pattern, loc };
}

struct regex_pattern regex_pattern(int sym, char *tag, char *pattern) {
    return regex_loc_pattern(sym, tag, pattern, regex_loc(NULL, 1, 1));
}

bool regex_null_pattern(struct regex_pattern const *pattern) {
    return pattern->pattern == NULL;
}

char const *str_for_regex_sym(enum regex_symbol type) {
    switch (type) {
        case RX_ERROR:             return "ERROR";

        case RX_EOF_T:             return "eof";
        case RX_CHAR_T:            return "a";
        case RX_RANGE_T:           return "a-z";
        case RX_NUM_T:             return "num";
        case RX_TAG_T:             return "tag";
        case RX_ALT_T:             return "|";
        case RX_STAR_T:            return "*";
        case RX_PLUS_T:            return "+";
        case RX_OPTIONAL_T:        return "?";
        case RX_DOTALL_T:          return ".";
        case RX_LPAREN_T:          return "(";
        case RX_RPAREN_T:          return ")";
        case RX_CLASS_T:           return "[";
        case RX_NEG_CLASS_T:       return "[^";
        case RX_END_CLASS_T:       return "]";
        case RX_TAG_BRACE_T:       return "{tag";
        case RX_LBRACE_T:          return "{";
        case RX_RBRACE_T:          return "}";

        case RX_REGEX_NT:          return "REGEX";
        case RX_EXPR_NT:           return "EXPR";
        case RX_ALT_NT:            return "ALT";
        case RX_ALTS_NT:           return "ALTS";
        case RX_FACTOR_NT:         return "FACTOR";
        case RX_FACTORS_NT:        return "FACTORS";
        case RX_CHAR_CLASS_NT:     return "CHAR_CLASS";
        case RX_RANGES_NT:         return "RANGES";
        case RX_UNOPS_NT:          return "UNOPS";

        case RX_DO_REGEX:          return "{regex}";
        case RX_DO_EMPTY:          return "{empty}";
        case RX_DO_ALT:            return "{alt}";
        case RX_DO_CAT:            return "{cat}";
        case RX_DO_SUB:            return "{sub}";
        case RX_DO_TAG:            return "{tag}";
        case RX_DO_CHAR_CLASS:     return "{char_class}";
        case RX_DO_NEG_CLASS:      return "{neg_class}";
        case RX_DO_DOTALL:         return "{dotall}";
        case RX_DO_CHAR:           return "{char}";
        case RX_DO_RANGES:         return "{ranges}";
        case RX_DO_RANGE:          return "{range}";
        case RX_DO_STAR:           return "{star}";
        case RX_DO_PLUS:           return "{plus}";
        case RX_DO_OPTIONAL:       return "{optional}";
        case RX_DO_REPEAT_EXACT:   return "{repeat_exact}";
    }
}

static void print_symbol_list(FILE *handle, enum regex_symbol const *sym) {
    if (*sym) {
        fprintf(handle, "%s", str_for_regex_sym(*sym));
        sym++;

        while (*sym) {
            fprintf(handle, ", %s", str_for_regex_sym(*sym));
            sym++;
        }
    }
}

#define SYNERR_FMT "| Regex Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNERR_FMT_AT "\n|\n| At: "
#define SYNERR_FMT_END "\n|\n"
#define OOM_FMT "| Regex OOM Error\n|\n"
#define REPEAT_ZERO_FMT "| Regex Repeat Zero Error\n|\n"
#define TAG_EXISTS_FMT "| Regex Tag Exists Error\n|\n| Pattern %s already tagged\n|\n"
#define MISSING_TAG_FMT "| Regex Missing Tag Error\n|\n| Pattern %s not tagged\n|\n"
#define DUPLICATE_PATTERN_FMT "| Regex Duplicate Pattern Error\n|\n| Pattern %s duplicated\n|\n"

void print_regex_error(FILE *handle, struct regex_error error) {
    switch (error.type) {
        case RX_SYNTAX_ERROR:
            fprintf(handle, SYNERR_FMT, str_for_regex_sym(error.actual));
            print_symbol_list(handle, error.expected);
            fprintf(handle, SYNERR_FMT_AT);
            print_regex_loc(handle, error.lexeme_loc);
            fprintf(handle, SYNERR_FMT_END);
            break;
        case RX_OUT_OF_MEMORY:
            fprintf(handle, OOM_FMT);
            break;
        case RX_MISSING_TAG:
            fprintf(handle, MISSING_TAG_FMT, error.tag);
            break;
        case RX_TAG_EXISTS:
            fprintf(handle, TAG_EXISTS_FMT, error.tag);
            break;
        case RX_REPEAT_ZERO:
            fprintf(handle, REPEAT_ZERO_FMT);
            break;
        case RX_DUPLICATE_PATTERN:
            fprintf(handle, DUPLICATE_PATTERN_FMT, error.pattern);
            break;
    }
}

void print_regex_loc(FILE *handle, struct regex_loc loc) {
    if (loc.path) fprintf(handle, "%s:", loc.path);
    fprintf(handle, "%d:%d", loc.line, loc.col);
}

void print_regex_range(FILE *handle, struct regex_char_range r) {
    fprintf(handle, "%c-%c", r.start, r.end);
}

