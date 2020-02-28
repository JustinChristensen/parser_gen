#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <base/debug.h>
#include <regex/nfa.h>
#include "gram/parser.h"
#include "gram/ast.h"

#define debug(...) debug_ns_("gram_parser", __VA_ARGS__);
#define tryinit(context, fn, ...) (sast(fn(__VA_ARGS__), context) || set_oom_error(context))

#define FIRST (enum gram_symbol[])
static enum gram_symbol *first_sets[] = {
    [GM_EOF_T]                = FIRST { GM_EOF_T, 0 },
    [GM_REGEX_T]              = FIRST { GM_REGEX_T, 0 },
    [GM_SECTION_T]            = FIRST { GM_SECTION_T, 0 },
    [GM_ASSIGN_T]             = FIRST { GM_ASSIGN_T, 0 },
    [GM_ALT_T]                = FIRST { GM_ALT_T, 0 },
    [GM_SEMICOLON_T]          = FIRST { GM_SEMICOLON_T, 0 },
    [GM_CHAR_T]               = FIRST { GM_CHAR_T, 0 },
    [GM_STRING_T]             = FIRST { GM_STRING_T, 0 },
    [GM_EMPTY_T]              = FIRST { GM_EMPTY_T, 0 },
    [GM_COMMENT_T]            = FIRST { GM_COMMENT_T, 0 },
    [GM_ID_T]                 = FIRST { GM_ID_T, 0 },
    [GM_WHITESPACE_T]         = FIRST { GM_WHITESPACE_T, 0 },

    [GM_PARSER_SPEC_NT]       = FIRST { GM_ID_T, GM_SECTION_T, GM_EOF_T,  0 },
    [GM_PATTERN_DEFS_NT]      = FIRST { GM_ID_T, GM_SECTION_T, GM_EOF_T, 0 },
    [GM_PATTERN_DEF_NT]       = FIRST { GM_ID_T, 0 },
    [GM_GRAMMAR_NT]           = FIRST { GM_SECTION_T, GM_EOF_T, 0 },
    [GM_RULES_NT]             = FIRST { GM_ID_T, GM_EOF_T, 0  },
    [GM_RULE_NT]              = FIRST { GM_ID_T, 0 },
    [GM_ALTS_NT]              = FIRST { GM_ALT_T, GM_SEMICOLON_T, 0  },
    [GM_ALT_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 },
    [GM_RHSES_NT]             = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, GM_ALT_T, GM_SEMICOLON_T, 0 },
    [GM_RHS_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 }
};
#undef FIRST

static enum gram_symbol *first_set(enum gram_symbol sym) {
    return first_sets[sym];
}
static char const *str_for_sym(enum gram_symbol sym) {
    switch (sym) {
        case GM_ERROR:                 return "ERROR";

        case GM_EOF_T:                 return "eof";
        case GM_REGEX_T:               return "/abc/";
        case GM_SECTION_T:             return "---";
        case GM_ASSIGN_T:              return "=";
        case GM_ALT_T:                 return "|";
        case GM_SEMICOLON_T:           return ";";
        case GM_CHAR_T:                return "'a'";
        case GM_STRING_T:              return "\"abc\"";
        case GM_EMPTY_T:               return "$empty";
        case GM_COMMENT_T:             return "//...";
        case GM_ID_T:                  return "id";
        case GM_WHITESPACE_T:          return " ";

        case GM_PARSER_SPEC_NT:        return "PARSER_SPEC";
        case GM_PATTERN_DEFS_NT:       return "PATTERN_DEFS";
        case GM_PATTERN_DEF_NT:        return "PATTERN_DEF";
        case GM_GRAMMAR_NT:            return "GRAMMAR";
        case GM_RULES_NT:              return "RULES";
        case GM_RULE_NT:               return "RULE";
        case GM_ALTS_NT:               return "ALTS";
        case GM_ALT_NT:                return "ALT";
        case GM_RHSES_NT:              return "RHSES";
        case GM_RHS_NT:                return "RHS";
    }
}

static void print_symbol_list(FILE *handle, enum gram_symbol *sym) {
    if (*sym) {
        fprintf(handle, "%s", str_for_sym(*sym));
        sym++;
        while (*sym) fprintf(handle, ", %s", str_for_sym(*sym)), sym++;
    }
}

static struct gram_error syntax_error(enum gram_symbol actual, struct regex_loc loc, enum gram_symbol expected) {
    return (struct gram_error) {
        .type = GM_SYNTAX_ERROR,
        .actual = actual,
        .loc = loc,
        .expected = first_set(expected)
    };
}

static bool set_syntax_error(enum gram_symbol expected, struct gram_parse_context *context) {
    if (!context->has_error) {
        debug("parse error %s\n", str_for_sym(expected));
        context->has_error = true;
        context->error = syntax_error(gram_lookahead(context), gram_location(context), expected);
    }

    return false;
}

static struct gram_error oom_error() {
    return (struct gram_error) { .type = GM_OOM_ERROR };
}

static bool set_oom_error(struct gram_parse_context *context) {
    if (!context->has_error) {
        context->has_error = true;
        context->error = oom_error();
    }

    return false;
}

static struct gram_error scanner_error(struct regex_error error) {
    return (struct gram_error) { .type = GM_SCANNER_ERROR, .scanerr = error };
}

static bool set_scanner_error(struct regex_error error, struct gram_parse_context *context) {
    if (!context->has_error) {
        context->has_error = true;
        context->error = scanner_error(error);
    }

    return false;
}

static bool peek(enum gram_symbol expected, struct gram_parse_context *context) {
    enum gram_symbol const *s = first_set(expected);

    while (*s) {
        if (gram_lookahead(context) == *s) return true;
        else s++;
    }

    return false;
}

static bool expect(enum gram_symbol expected, struct gram_parse_context *context) {
    if (peek(expected, context)) {
        debug("success, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(context->sym));
        context->sym = gram_scan(context);
        return true;
    }

    debug("failure, expected \"%s\", actual \"%s\"\n",
        str_for_sym(expected), str_for_sym(context->sym));

    return set_syntax_error(expected, context);
}

bool gram_parse_context(struct gram_parse_context *context) {
    struct nfa_context scanner;

    if (!nfa_context(&scanner, RX_PATTERNS {
        RX_ALPHA_(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
        RX_LINE_COMMENT(GM_COMMENT_T),
        RX_REGEX(GM_REGEX_T),
        { GM_SECTION_T, NULL, "---\n" },
        { GM_ASSIGN_T, NULL, "=" },
        { GM_ALT_T, NULL, "\\|" },
        { GM_SEMICOLON_T, NULL, ";" },
        { GM_EMPTY_T, NULL, "$empty" },
        { GM_CHAR_T, NULL, "'(\\\\.|[^'])'" }, // 7
        { GM_STRING_T, NULL, "\"(\\\\.|[^\"])*\"" }, // 8
        { GM_ID_T, NULL, "{alpha_}{alnum_}*" },
        RX_SPACE(GM_WHITESPACE_T),
        RX_END_PATTERNS
    })) {
        set_scanner_error(nfa_error(&scanner), context);
        free_nfa_context(&scanner);
        return false;
    }

    *context = (struct gram_parse_context) { .scanner = scanner };

    return true;
}

void free_gram_parse_context(struct gram_parse_context *context) {
    free_nfa_match(&context->match);
    free_nfa_context(&context->scanner);
    if (context->ast) {
        free_gram_parser_spec(context->ast);
        context->ast = NULL;
    }
}

bool gram_parser_has_error(struct gram_parse_context *context) {
    return context->has_error;
}

struct gram_error gram_parser_error(struct gram_parse_context *context) {
    return context->error;
}

#define SYNTAX_ERROR_FMT_START "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
#define SYNTAX_ERROR_FMT_END "\n|\n"
#define OOM_ERROR_FMT "| Out of Memory Error\n|\n"

void print_gram_error(FILE *handle, struct gram_error error) {
    switch (error.type) {
        case GM_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, str_for_sym(error.actual));
            print_symbol_list(handle, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, SYNTAX_ERROR_FMT_END);
            break;
        case GM_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT);
            break;
        case GM_SCANNER_ERROR:
            print_regex_error(stderr, error.scanerr);
            break;
    }
}

bool gram_start_scanning(char *input, struct gram_parse_context *context) {
    struct nfa_match match = { 0 };

    if (nfa_start_match(input, &match, &context->scanner)) {
        context->match = match;
        context->sym = gram_scan(context);
        context->has_error = false;
        return true;
    }

    return set_oom_error(context);
}

enum gram_symbol gram_scan(struct gram_parse_context *context) {
    enum gram_symbol sym = gram_lookahead(context);
    do sym = nfa_match(&context->match);
    while (sym == GM_WHITESPACE_T || sym == GM_COMMENT_T);
    return sym == RX_EOF ? GM_EOF_T : sym;
}

enum gram_symbol gram_lookahead(struct gram_parse_context *context) {
    return context->sym;
}

struct regex_loc gram_location(struct gram_parse_context *context) {
    return nfa_match_loc(&context->match);
}

bool gram_lexeme(char *lexeme, struct gram_parse_context *context) {
    nfa_match_lexeme(lexeme, &context->match);
    return true;
}

static void *gast(struct gram_parse_context *context) {
    return context->ast;
}

static bool sast(void *ast, struct gram_parse_context *context) {
    context->ast = ast;
    return ast ? true : false;
}

static bool strip_quotes(char *lexeme) {
    if (!lexeme || lexeme[0] == '\0' || lexeme[1] == '\0') return true;

    int i = 0;
    for (; lexeme[i + 2] != '\0'; i++) {
        lexeme[i] = lexeme[i + 1];
    }
    lexeme[i] = '\0';

    return true;
}

struct gram_parser_spec *get_gram_parser_spec(struct gram_parse_context *context) {
    return gast(context);
}

static bool parse_pattern_defs_head(struct gram_parse_context *context);
static bool parse_pattern_defs(struct gram_parse_context *context);
static bool parse_pattern_def(struct gram_parse_context *context);
static bool parse_grammar(struct gram_parse_context *context);
static bool parse_rules_head(struct gram_parse_context *context);
static bool parse_rules(struct gram_parse_context *context);
static bool parse_rule(struct gram_parse_context *context);
static bool parse_alts(struct gram_parse_context *context);
static bool parse_alt(struct gram_parse_context *context);
static bool parse_rhses(struct gram_parse_context *context);
static bool parse_rhs(struct gram_parse_context *context);

bool parse_gram_parser_spec(char *spec, struct gram_parse_context *context) {
    if (!gram_start_scanning(spec, context)) return false;

    if (parse_pattern_defs_head(context)) {
        struct gram_pattern_def *pdefs = gast(context);

        if (parse_grammar(context) &&
            expect(GM_EOF_T, context) &&
            tryinit(context, init_gram_parser_spec, pdefs, gast(context))) return true;
        else if (pdefs) {
            free_gram_pattern_def(pdefs);
            sast(NULL, context);
        }
    }

    return set_syntax_error(GM_PARSER_SPEC_NT, context);
}

static bool parse_pattern_defs_head(struct gram_parse_context *context) {
    if (peek(GM_SECTION_T, context) || peek(GM_EOF_T, context))
        return true; // ε

    if (parse_pattern_def(context)) {
        struct gram_pattern_def *defs = gast(context);

        if (parse_pattern_defs(context)) {
            sast(defs, context);
            return true;
        } else if (defs) {
            free_gram_pattern_def(defs);
            return sast(NULL, context);
        }
    }

    return set_syntax_error(GM_PATTERN_DEFS_NT, context);
}

static bool parse_pattern_defs(struct gram_parse_context *context) {
    if (peek(GM_SECTION_T, context) || peek(GM_EOF_T, context))
        return true; // ε

    struct gram_pattern_def *prev_def = gast(context);

    if (parse_pattern_def(context)) {
        prev_def->next = gast(context);
        if (parse_pattern_defs(context)) return true;
    }

    return set_syntax_error(GM_PATTERN_DEFS_NT, context);
}

static bool parse_pattern_def(struct gram_parse_context *context) {
    char idbuf[BUFSIZ] = "";

    if (gram_lexeme(idbuf, context) && expect(GM_ID_T, context)) {
        char patbuf[BUFSIZ] = "";

        if (gram_lexeme(patbuf, context) &&
            expect(GM_REGEX_T, context) &&
            tryinit(context, init_gram_pattern_def, idbuf, patbuf, NULL))
            return true;
    }

    return set_syntax_error(GM_PATTERN_DEF_NT, context);
}

static bool parse_grammar(struct gram_parse_context *context) {
    if (peek(GM_EOF_T, context)) return true; // ε

    if (expect(GM_SECTION_T, context) && parse_rules_head(context))
        return true;

    return set_syntax_error(GM_GRAMMAR_NT, context);
}

static bool parse_rules_head(struct gram_parse_context *context) {
    if (peek(GM_EOF_T, context)) return true; // ε

    if (parse_rule(context)) {
        struct gram_rule *rules = gast(context);

        if (parse_rules(context)) {
            sast(rules, context);
            return true;
        } else if (rules) {
            free_gram_rule(rules);
            return sast(NULL, context);
        }
    }

    return set_syntax_error(GM_RULES_NT, context);
}

static bool parse_rules(struct gram_parse_context *context) {
    if (peek(GM_EOF_T, context)) return true; // ε

    struct gram_rule *rule = gast(context);

    if (parse_rule(context)) {
        rule->next = gast(context);
        if (parse_rules(context)) return true;
    }

    return set_syntax_error(GM_RULES_NT, context);
}

static bool parse_rule(struct gram_parse_context *context) {
    char idbuf[BUFSIZ] = "";

    if (gram_lexeme(idbuf, context) && expect(GM_ID_T, context) && expect(GM_ASSIGN_T, context) && parse_alt(context)) {
        struct gram_alt *alts = gast(context);

        if (parse_alts(context) &&
            expect(GM_SEMICOLON_T, context) &&
            tryinit(context, init_gram_rule, idbuf, alts, NULL))
            return true;
        else if (alts) {
            free_gram_alt(alts);
            return sast(NULL, context);
        }
    }

    return set_syntax_error(GM_RULE_NT, context);
}

static bool parse_alts(struct gram_parse_context *context) {
    if (peek(GM_SEMICOLON_T, context)) return true; // ε

    struct gram_alt *prev_alt = gast(context);

    if (expect(GM_ALT_T, context) && parse_alt(context)) {
        prev_alt->next = gast(context);
        if (parse_alts(context)) return true;
    }

    return set_syntax_error(GM_ALTS_NT, context);
}

static bool parse_alt(struct gram_parse_context *context) {
    if (parse_rhs(context)) {
        struct gram_rhs *rhses = gast(context);

        if (parse_rhses(context) && tryinit(context, init_gram_alt, rhses, NULL))
            return true;
        else if (rhses) {
            free_gram_rhs(rhses);
            return sast(NULL, context);
        }
    }

    return set_syntax_error(GM_ALT_NT, context);
}

static bool parse_rhses(struct gram_parse_context *context) {
    if (peek(GM_ALT_T, context) || peek(GM_SEMICOLON_T, context))
        return true; // ε

    struct gram_rhs *prev_rhs = gast(context);

    if (parse_rhs(context)) {
        prev_rhs->next = gast(context);
        if (parse_rhses(context)) return true;
    }

    return set_syntax_error(GM_RHSES_NT, context);
}

static bool parse_rhs(struct gram_parse_context *context) {
    char symbuf[BUFSIZ] = "";

    if (peek(GM_ID_T, context) &&
        gram_lexeme(symbuf, context) &&
        tryinit(context, init_id_gram_rhs, symbuf, NULL) &&
        expect(GM_ID_T, context)) return true;
    else if (peek(GM_CHAR_T, context) &&
        gram_lexeme(symbuf, context) &&
        strip_quotes(symbuf) &&
        tryinit(context, init_char_gram_rhs, symbuf, NULL) &&
        expect(GM_CHAR_T, context)) return true;
    else if (peek(GM_STRING_T, context) &&
        gram_lexeme(symbuf, context) &&
        strip_quotes(symbuf) &&
        tryinit(context, init_string_gram_rhs, symbuf, NULL) &&
        expect(GM_STRING_T, context)) return true;
    else if (peek(GM_EMPTY_T, context) &&
        tryinit(context, init_empty_gram_rhs, NULL) &&
        expect(GM_EMPTY_T, context)) return true;

    return set_syntax_error(GM_RHS_NT, context);
}

void print_gram_tokens(FILE *handle, char *spec) {
    struct gram_parse_context context = { 0 };

    if (gram_parse_context(&context) && gram_start_scanning(spec, &context)) {
        enum gram_symbol sym = gram_lookahead(&context);
        char buf[BUFSIZ];

        fprintf(handle, "%3s\t%9s\t%s\n", "sym", "location", "token");

        do {
            struct regex_loc loc = gram_location(&context);
            gram_lexeme(buf, &context);
            fprintf(handle, "%3d\t%4d:%-4d\t%s\n", sym, loc.line, loc.col, buf);
        } while ((sym = gram_scan(&context)) != GM_EOF_T);

        free_gram_parse_context(&context);
    }
}

