#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "parser.h"
#include "regex/nfa.h"

#define FIRST (enum gram_symbol[])
static enum gram_symbol *first_sets[] = {
    [GM_REGEX_T]           = FIRST { GM_REGEX_T, 0 },
    [GM_SECTION_T]         = FIRST { GM_SECTION_T, 0 },
    [GM_ASSIGN_T]          = FIRST { GM_ASSIGN_T, 0 },
    [GM_ALT_T]             = FIRST { GM_ALT_T, 0 },
    [GM_SEMICOLON_T]       = FIRST { GM_SEMICOLON_T, 0 },
    [GM_CHAR_T]            = FIRST { GM_CHAR_T, 0 },
    [GM_STRING_T]          = FIRST { GM_STRING_T, 0 },
    [GM_EMPTY_T]           = FIRST { GM_EMPTY_T, 0 },
    [GM_COMMENT_T]         = FIRST { GM_COMMENT_T, 0 },
    [GM_ID_T]              = FIRST { GM_ID_T, 0 },
    [GM_WHITESPACE_T]      = FIRST { GM_WHITESPACE_T, 0 },

    [GM_PARSER_SPEC_NT]       = FIRST { GM_ID_T, GM_SECTION_T, RX_EOF,  0 },
    [GM_PATTERN_DEFS_HEAD_NT] = FIRST { GM_ID_T, GM_SECTION_T, RX_EOF, 0 },
    [GM_PATTERN_DEFS_NT]      = FIRST { GM_ID_T, GM_SECTION_T, RX_EOF, 0 },
    [GM_PATTERN_DEF_NT]       = FIRST { GM_ID_T, 0 },
    [GM_GRAMMAR_NT]           = FIRST { GM_SECTION_T, RX_EOF, 0 },
    [GM_RULES_HEAD_NT]        = FIRST { GM_ID_T, RX_EOF, 0  },
    [GM_RULES_NT]             = FIRST { GM_ID_T, RX_EOF, 0  },
    [GM_RULE_NT]              = FIRST { GM_ID_T, 0 },
    [GM_ALTS_HEAD_NT]         = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0  },
    [GM_ALTS_NT]              = FIRST { GM_ALT_T, GM_SEMICOLON_T, 0  },
    [GM_ALT_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 },
    [GM_RHSES_NT]             = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, GM_ALT_T, GM_SEMICOLON_T, 0 },
    [GM_RHS_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 }
};
#undef FIRST

static union gram_result id_result(char *id)
    { return (union gram_result) { .id = id }; }
static union gram_result lit_result(char *lit)
    { return (union gram_result) { .lit = lit }; }
static union gram_result pdef_result(char *id, char *regex)
    { return (union gram_result) { .pdef = { id, regex } }; }

static enum gram_symbol *first_set(enum gram_symbol sym) {
    return first_sets[sym];
}
static char const *str_for_sym(enum gram_symbol sym) {
    switch (sym) {
        case GM_ERROR:                 return "ERROR";

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
        case GM_PATTERN_DEFS_HEAD_NT:  return "PATTERN_DEFS_HEAD";
        case GM_PATTERN_DEFS_NT:       return "PATTERN_DEFS";
        case GM_PATTERN_DEF_NT:        return "PATTERN_DEF";
        case GM_GRAMMAR_NT:            return "GRAMMAR";
        case GM_RULES_HEAD_NT:         return "RULES_HEAD";
        case GM_RULES_NT:              return "RULES";
        case GM_RULE_NT:               return "RULE";
        case GM_ALTS_HEAD_NT:          return "ALTS_HEAD";
        case GM_ALTS_NT:               return "ALTS";
        case GM_ALT_NT:                return "ALT";
        case GM_RHSES_NT:              return "RHSES";
        case GM_RHS_NT:                return "RHS";

        case GM_DO_PARSER_SPEC:        return "{parser_spec}";
        case GM_DO_PATTERN_DEF:        return "{pattern_def}";
        case GM_DO_APPEND_PATTERN_DEF: return "{+= pattern_def}";
        case GM_DO_PATTERN_DEFS_HEAD:  return "{pattern_defs_head}";
        case GM_DO_RULE:               return "{rule}";
        case GM_DO_APPEND_RULE:        return "{+= rule}";
        case GM_DO_RULES_HEAD:         return "{rules_head}";
        case GM_DO_ALT:                return "{alt}";
        case GM_DO_APPEND_ALT:         return "{+= alt}";
        case GM_DO_ALTS_HEAD:          return "{alts_head}";
        case GM_DO_ID_RHS:             return "{id_rhs}";
        case GM_DO_LIT_RHS:            return "{lit_rhs}";
        case GM_DO_EMPTY_RHS:          return "{empty_rhs}";
        case GM_DO_APPEND_RHS:         return "{+= rhs}";
        case GM_DO_RHSES_HEAD:         return "{rhses_head}";
    }
}

static void print_symbol_list(FILE *handle, enum gram_symbol *sym) {
    if (*sym) {
        fprintf(handle, "%s", str_for_sym(*sym));
        sym++;
        while (*sym) fprintf(handle, ", %s", str_for_sym(*sym)), sym++;
    }
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
        context->sym = gram_scan(context);
        return true;
    }

    return false;
}

static bool do_action(enum gram_symbol action, union gram_result val, struct gram_parse_context *context) {
    return context->pi.actions[AI(action)](val, context->result);
}

static union gram_result result(struct gram_parse_context *context) {
    return context->pi.result(context->result);
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
        context->has_error = true;
        context->error = syntax_error(gram_lookahead(context), gram_location(context), expected);
    }

    return false;
}

bool gm_parse_context(
    struct gram_parse_context *context,
    struct gram_parse_interface pi,
    void *result
) {
    struct nfa_context scanner;

    if (!nfa_context(&scanner, RX_PATTERNS {
        RX_ALPHA_(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
        RX_LINE_COMMENT(GM_COMMENT_T),
        RX_REGEX(GM_REGEX_T),
        { GM_SECTION_T, NULL, "\n---\n" },
        { GM_ASSIGN_T, NULL, "=" },
        { GM_ALT_T, NULL, "|" },
        { GM_SEMICOLON_T, NULL, ";" },
        { GM_EMPTY_T, NULL, "$empty" },
        { GM_CHAR_T, NULL, "'(\\\\.|[^'\\\\])*'" },
        { GM_STRING_T, NULL, "\"(\\\\.|[^\"\\\\])*\"" },
        { GM_ID_T, NULL, "{alpha_}{alnum_}*" },
        RX_SPACE(GM_WHITESPACE_T),
        RX_END_PATTERNS
    })) {
        free_nfa_context(&scanner);
        return false;
    }

    *context = (struct gram_parse_context) {
        .result = result,
        .pi = pi,
        .scanner = scanner
    };

    return true;
}

bool gm_parser_has_error(struct gram_parse_context *context) {
    return context->has_error;
}

struct gram_error gm_parser_error(struct gram_parse_context *context) {
    return context->error;
}

void gm_print_error(struct gram_error error) {
    switch (error.type) {
        case GM_SYNTAX_ERROR:
            fprintf(stderr, SYNTAX_ERROR_FMT_START, str_for_sym(error.actual));
            print_symbol_list(stderr, error.expected);
            fprintf(stderr, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(stderr, error.loc);
            fprintf(stderr, SYNTAX_ERROR_FMT_END);
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

    return false;
}

enum gram_symbol gram_scan(struct gram_parse_context *context) {
    enum gram_symbol sym = gram_lookahead(context);
    do sym = nfa_match(&context->match);
    while (sym == GM_WHITESPACE_T);
    return sym;
}

enum gram_symbol gram_lookahead(struct gram_parse_context *context) {
    return context->sym;
}

struct regex_loc gram_location(struct gram_parse_context *context) {
    return nfa_match_loc(&context->match);
}

char *gram_lexeme(struct gram_parse_context *context) {
    static char buf[BUFSIZ] = "";
    nfa_match_lexeme(buf, &context->match);
    return buf;
}

static bool parse_pattern_defs_head(struct gram_parse_context *context);
static bool parse_pattern_defs(struct gram_parse_context *context);
static bool parse_pattern_def(struct gram_parse_context *context);
static bool parse_grammar(struct gram_parse_context *context);
static bool parse_rules_head(struct gram_parse_context *context);
static bool parse_rules(struct gram_parse_context *context);
static bool parse_rule(struct gram_parse_context *context);
static bool parse_alts_head(struct gram_parse_context *context);
static bool parse_alts(struct gram_parse_context *context);
static bool parse_alt(struct gram_parse_context *context);
static bool parse_rhses(struct gram_parse_context *context);
static bool parse_rhs(struct gram_parse_context *context);

bool gram_parse_parser_spec(char *spec, struct gram_parse_context *context) {
    gram_start_scanning(spec, context);

    if (parse_pattern_defs_head(context)) {
        union gram_result pdefs = result(context);

        if (parse_grammar(context) &&
            expect(RX_EOF, context) &&
            do_action(GM_DO_PARSER_SPEC, pdefs, context)) return true;
    }

    return set_syntax_error(GM_PARSER_SPEC_NT, context);
}

static bool parse_pattern_defs_head(struct gram_parse_context *context) {
    if (peek(GM_SECTION_T, context) || peek(RX_EOF, context)) {
        return true; // ε
    } else if (parse_pattern_def(context)) {
        union gram_result head = result(context);

        if (parse_pattern_defs(context) &&
            do_action(GM_DO_PATTERN_DEFS_HEAD, head, context)) return true;
    }

    return set_syntax_error(GM_PATTERN_DEFS_HEAD_NT, context);
}

static bool parse_pattern_defs(struct gram_parse_context *context) {
    if (peek(GM_SECTION_T, context) || peek(RX_EOF, context)) {
        return true; // ε
    } else {
        union gram_result prev = result(context);

        if (parse_pattern_def(context) &&
            do_action(GM_DO_APPEND_PATTERN_DEF, prev, context) &&
            parse_pattern_defs(context)) return true;
    }

    return set_syntax_error(GM_PATTERN_DEFS_NT, context);
}

static bool parse_pattern_def(struct gram_parse_context *context) {
    union gram_result id = id_result(gram_lexeme(context));

    if (expect(GM_ID_T, context)) {
        union gram_result pdef = pdef_result(id.id, gram_lexeme(context));

        if (expect(GM_REGEX_T, context) &&
            do_action(GM_DO_PATTERN_DEF, pdef, context)) return true;
    }

    return set_syntax_error(GM_PATTERN_DEF_NT, context);
}

static bool parse_grammar(struct gram_parse_context *context) {
    if (peek(RX_EOF, context)) {
        return true; // ε
    } else if (expect(GM_SECTION_T, context) && parse_rules_head(context)) {
        return true;
    }

    return set_syntax_error(GM_GRAMMAR_NT, context);
}

static bool parse_rules_head(struct gram_parse_context *context) {
    if (peek(RX_EOF, context)) {
        return true; // ε
    } else if (parse_rule(context)) {
        union gram_result head = result(context);

        if (parse_rules(context) && do_action(GM_DO_RULES_HEAD, head, context))
            return true;
    }

    return set_syntax_error(GM_RULES_HEAD_NT, context);
}

static bool parse_rules(struct gram_parse_context *context) {
    if (peek(RX_EOF, context)) {
        return true; // ε
    } else {
        union gram_result prev = result(context);

        if (parse_rule(context) &&
            do_action(GM_DO_APPEND_RULE, prev, context) &&
            parse_rules(context)) return true;
    }

    return set_syntax_error(GM_RULES_NT, context);
}

static bool parse_rule(struct gram_parse_context *context) {
    union gram_result id = id_result(gram_lexeme(context));

    if (expect(GM_ID_T, context) &&
        expect(GM_ALT_T, context) &&
        parse_alts_head(context) &&
        expect(GM_SEMICOLON_T, context) &&
        do_action(GM_DO_RULE, id, context)) return true;

    return set_syntax_error(GM_RULE_NT, context);
}

static bool parse_alts_head(struct gram_parse_context *context) {
    if (parse_alt(context)) {
        union gram_result head = result(context);

        if (parse_alts(context) && do_action(GM_DO_ALTS_HEAD, head, context))
            return true;
    }

    return set_syntax_error(GM_ALTS_HEAD_NT, context);
}

static bool parse_alts(struct gram_parse_context *context) {
    if (peek(GM_SEMICOLON_T, context)) {
        return true; // ε
    } else {
        union gram_result prev = result(context);

        if (expect(GM_ALT_T, context) &&
            parse_alt(context) &&
            do_action(GM_DO_APPEND_ALT, prev, context) &&
            parse_alts(context)) return true;
    }

    return set_syntax_error(GM_ALTS_NT, context);
}

static bool parse_alt(struct gram_parse_context *context) {
    if (parse_rhs(context)) {
        union gram_result head = result(context);

        if (parse_rhses(context) &&
            do_action(GM_DO_RHSES_HEAD, head, context) &&
            do_action(GM_DO_ALT, NULL_RESULT, context)) return true;
    }

    return set_syntax_error(GM_ALT_NT, context);
}

static bool parse_rhses(struct gram_parse_context *context) {
    if (peek(GM_ALT_T, context) || peek(GM_SEMICOLON_T, context)) {
        return true; // ε
    } else {
        union gram_result prev = result(context);

        if (parse_rhs(context) &&
            do_action(GM_DO_APPEND_RHS, prev, context) &&
            parse_rhses(context)) return true;
    }

    return set_syntax_error(GM_RHSES_NT, context);
}

static bool parse_rhs(struct gram_parse_context *context) {
    if (peek(GM_ID_T, context) &&
        do_action(GM_DO_ID_RHS, id_result(gram_lexeme(context)), context) &&
        expect(GM_ID_T, context)) return true;
    else if (peek(GM_CHAR_T, context) &&
        do_action(GM_DO_LIT_RHS, lit_result(gram_lexeme(context)), context) &&
        expect(GM_CHAR_T, context)) return true;
    else if (peek(GM_STRING_T, context) &&
        do_action(GM_DO_LIT_RHS, lit_result(gram_lexeme(context)), context) &&
        expect(GM_STRING_T, context)) return true;
    else if (peek(GM_EMPTY_T, context) &&
        do_action(GM_DO_EMPTY_RHS, NULL_RESULT, context) &&
        expect(GM_EMPTY_T, context)) return true;

    return set_syntax_error(GM_RHS_NT, context);
}

