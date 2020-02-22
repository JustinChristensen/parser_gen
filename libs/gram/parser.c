#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "gram/parser.h"
#include "regex/nfa.h"

#define FIRST (enum gram_symbol[])
static enum gram_symbol *first_sets[] {
    [GM_REGEX_T]           = FIRST { GM_REGEX_T, 0 },
    [GM_SECTION_T]         = FIRST { GM_SECTION_T, 0 },
    [GM_ASSIGN_T]          = FIRST { GM_ASSIGN_T, 0 },
    [GM_ALT_T]             = FIRST { GM_ALT_T, 0 },
    [GM_SEMICOLON_T]       = FIRST { GM_SEMICOLON_T, 0 },
    [GM_CHAR_T]            = FIRST { GM_CHAR_T, 0 },
    [GM_STRING_T]          = FIRST { GM_STRING_T, 0 },
    [GM_EMPTY_T]           = FIRST { GM_EMPTY_T, 0 },
    [GM_COMMENT_T]         = FIRST { GM_COMMENT_T, 0 },
    [GM_ID_T]              = FIRST { GM_ID_T, 0 }
    [GM_WHITESPACE_T]      = FIRST { GM_WHITESPACE_T, 0 }

    [PARSER_SPEC_NT]       = FIRST { GM_ID_T, GM_SECTION_T, RE_EOF,  0 },
    [PATTERN_DEFS_HEAD_NT] = FIRST { GM_ID_T, GM_SECTION_T, RE_EOF, 0 },
    [PATTERN_DEFS_NT]      = FIRST { GM_ID_T, GM_SECTION_T, RE_EOF, 0 },
    [PATTERN_DEF_NT]       = FIRST { GM_ID_T, 0 },
    [GRAMMAR_NT]           = FIRST { GM_SECTION_T, RE_EOF, 0 },
    [RULES_HEAD_NT]        = FIRST { GM_ID_T, RE_EOF, 0  },
    [RULES_NT]             = FIRST { GM_ID_T, RE_EOF, 0  },
    [RULE_NT]              = FIRST { GM_ID_T, 0 },
    [ALTS_HEAD_NT]         = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0  },
    [ALTS_NT]              = FIRST { GM_ALT_T, GM_SEMICOLON_T, 0  },
    [ALT_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 },
    [RHSES_NT]             = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, GM_ALT_T, GM_SEMICOLON_T, 0 },
    [RHS_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 }
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

char const *str_for_sym(enum gram_symbol sym) {
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
        if (gm_lookahead(context) == *s) return true;
        else s++;
    }

    return false;
}

static bool expect(enum gram_symbol expected, struct gram_parse_context *context) {
    if (peek(expected, context)) {
        context->sym = gm_scan(context);
        return true;
    }

    return false;
}

static bool do_action(enum gram_symbol action, union gram_result val, struct gram_parse_context *context) {
    return context->actions[GM_AI(action)](val, context->result_context);
}

static union gram_result result(struct gram_parse_context *context) {
    return context->get_result(context->result_context);
}

static bool set_syntax_error(enum gram_symbol expected, struct gram_parse_context *context) {
    if (!context->has_error) {
        context->has_error = true;
        context->error = gm_syntax_error(gm_lookahead(context), gm_location(context), expected);
    }

    return false;
}

static struct gram_error syntax_error(enum gram_symbol actual, struct regex_loc loc, enum gram_symbol expected) {
    return (struct gram_error) {
        .type = SYNTAX_ERROR,
        .actual = actual,
        .loc = loc,
        .expected = first_set(expected)
    };
}

bool gm_parse_context(
    struct gram_parse_context *context,
    void *result,
    void (**actions)(union gram_result val, void *result),
    union gram_result (*get_result)(void *result),
) {
    struct nfa_context scanner;

    if (!re_nfa_context(&scanner, RE_PATTERNS {
        RE_ALPHA_(RE_TAG_ONLY), RE_ALNUM_(RE_TAG_ONLY),
        RE_LINE_COMMENT(COMMENT_T),
        RE_REGEX(REGEX_T),
        { SECTION_T, NULL, "\n---\n" },
        { ASSIGN_T, NULL, "=" },
        { ALT_T, NULL, "|" },
        { SEMICOLON_T, NULL, ";" },
        { EMPTY_T, NULL, "$empty" },
        { CHAR_T, NULL, "'(\\\\.|[^'\\\\])*'" },
        { STRING_T, NULL, "\"(\\\\.|[^\"\\\\])*\"" },
        { ID_T, NULL, "{alpha_}{alnum_}*" }
        RE_SPACE(WHITESPACE_T),
        RE_END_PATTERNS
    })) return false;

    *context = (struct gram_parse_context) {
        .result = result,
        .actions = actions,
        .get_result = get_result,
        .scanner = scanner
    };

    return true;
}

bool gm_parse_has_error(struct gram_parse_context *context) {
    return context->has_error;
}

struct gram_error gm_parse_error(struct gram_parse_context *context) {
    return context->error;
}

void gm_print_error(struct gram_error error) {
    switch (error.type) {
        case SYNTAX_ERROR:
            fprintf(strderr, GM_SYNTAX_ERROR_FMT_START, error.actual);
            print_symbol_list(stderr, error.expected);
            fprintf(stderr, GM_SYNTAX_ERROR_FMT_LOC);
            re_print_loc(error.loc);
            fprintf(stderr, GM_SYNTAX_ERROR_FMT_END);
            break;
    }
}

bool gm_start_scanning(char *input, struct gram_parse_context *context) {
    struct nfa_match_state match = { 0 };

    if (re_nfa_match_state(input, context->scanner)) {
        context->match = match;
        context->sym = gm_scan(context);
        context->has_error = false;
        return true;
    }

    return false;
}

enum gram_symbol gm_scan(struct gram_parse_context *context) {
    enum gram_symbol sym = gm_lookahead(context);
    do sym = re_nfa_match(&context->match_state);
    while (sym == WHITESPACE_T);
    return sym;
}

enum gram_symbol gm_lookahead(struct gram_parse_context *context) {
    return context->sym;
}

struct regex_loc gm_location(struct gram_parse_context *context) {
    return re_nfa_match_loc(&context->match);
}

char *gm_lexeme(struct gram_parse_context *context) {
    static char buf[BUFSIZ] = "";
    re_nfa_match_lexeme(buf, &context->match);
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

bool gm_parse_parser_spec(char *input, struct gram_parse_context *context) {
    gm_start_scanning(input, context);

    if (parse_pattern_defs_head(context)) {
        union gram_result pdefs = result(context);

        if (parse_grammar(context) &&
            expect(RE_EOF, context) &&
            do_action(GM_DO_PARSER_SPEC, pdefs, context)) return true;
    }

    return set_syntax_error(PARSER_SPEC_NT, context);
}

bool parse_pattern_defs_head(struct gram_parse_context *context) {
    if (peek(SECTION_T, context) || peek(EOF_T, context)) {
        return true; // ε
    } else if (parse_pattern_def(context)) {
        union gram_result head = result(context);

        if (parse_pattern_defs(context) &&
            do_action(GM_DO_PATTERN_DEFS_HEAD, head, context)) return true;
    }

    return set_syntax_error(PATTERN_DEFS_HEAD_NT, context);
}

bool parse_pattern_defs(struct gram_parse_context *context) {
    if (peek(SECTION_T, context) || peek(EOF_T, context)) {
        return true; // ε
    } else {
        union gram_result prev = result(context);

        if (parse_pattern_def(context) &&
            do_action(GM_DO_APPEND_PATTERN_DEF, prev, context) &&
            parse_pattern_defs(context)) return true;
    }

    return set_syntax_error(PATTERN_DEFS_NT, context);
}

bool parse_pattern_def(struct gram_parse_context *context) {
    union gram_result id = id_result(gm_lexeme(context));

    if (expect(ID_T, context)) {
        union gram_result pdef = pdef_result(id.id, gm_lexeme(context));

        if (expect(REGEX_T, context) &&
            do_action(GM_DO_PATTERN_DEF, pdef, context)) return true;
    }

    return set_syntax_error(PATTERN_DEF_NT, context);
}

bool parse_grammar(struct gram_parse_context *context) {
    if (peek(EOF_T, context)) {
        return true; // ε
    } else if (expect(SECTION_T, context) && parse_rules_head(context)) {
        return true;
    }

    return set_syntax_error(GRAMMAR_NT, context);
}

bool parse_rules_head(struct gram_parse_context *context) {
    if (peek(EOF_T, context)) {
        return true; // ε
    } else if (parse_rule(context)) {
        union gram_result head = result(context);

        if (parse_rules(context) && do_action(GM_DO_RULES_HEAD, head, context))
            return true;
    }

    return set_syntax_error(RULES_HEAD_NT, context);
}

bool parse_rules(struct gram_parse_context *context) {
    if (peek(EOF_T, context)) {
        return true; // ε
    } else {
        union gram_result prev = result(context);

        if (parse_rule(context) &&
            do_action(GM_DO_APPEND_RULE, prev, context) &&
            parse_rules(context)) return true;
    }

    return set_syntax_error(RULES_NT, context);
}

bool parse_rule(struct gram_parse_context *context) {
    union gram_result id = id_result(gm_lexeme(context));

    if (expect(GM_ID_T, context) &&
        expect(GM_ALT_T, context) &&
        parse_alts_head(context) &&
        expect(GM_SEMICOLON_T, context) &&
        do_action(GM_DO_RULE, id, context)) return true;

    return set_syntax_error(GM_RULE_NT, context);
}

bool parse_alts_head(struct gram_parse_context *context) {
    if (parse_alt(context)) {
        union gram_result head = result(context);

        if (parse_alts(context) && do_action(GM_DO_ALTS_HEAD, head, context))
            return true;
    }

    return set_syntax_error(GM_ALTS_HEAD_NT, context);
}

bool parse_alts(struct gram_parse_context *context) {
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

bool parse_alt(struct gram_parse_context *context) {
    if (parse_rhs(context)) {
        union gram_result head = result(context);

        if (parse_rhses(context) &&
            do_action(GM_DO_RHSES_HEAD, head, context) &&
            do_action(GM_DO_ALT, GM_NULL_RESULT, context)) return true;
    }

    return set_syntax_error(GM_ALT_NT, context);
}

bool parse_rhses(struct gram_parse_context *context) {
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

bool parse_rhs(struct gram_parse_context *context) {
    if (peek(GM_ID_T, context) &&
        do_action(GM_DO_ID_RHS, id_result(gm_lexeme(context)), context) &&
        expect(GM_ID_T, context)) return true;
    else if (peek(GM_CHAR_T, context) &&
        do_action(GM_DO_LIT_RHS, lit_result(gm_lexeme(context)), context) &&
        expect(GM_CHAR_T, context)) return true;
    else if (peek(GM_STRING_T, context) &&
        do_action(GM_DO_LIT_RHS, lit_result(gm_lexeme(context)), context) &&
        expect(GM_STRING_T, context)) return true;
    else if (peek(GM_EMPTY_T, context) &&
        do_action(GM_DO_EMPTY_RHS, GM_NULL_RESULT, context) &&
        expect(GM_EMPTY_T, context)) return true;

    return set_syntax_error(GM_RHS_NT, context);
}

