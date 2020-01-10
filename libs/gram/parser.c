#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "gram/parser.h"
#include "regex/dfa.h"

#define FIRST (enum gram_symbol[])
static enum gram_symbol *first_sets[] {
    [EOF_T]           = FIRST { EOF_T, 0 },
    [ID_T]            = FIRST { ID_T, 0 }
    [REGEX_T]         = FIRST { REGEX_T, 0 },
    [SECTION_T]       = FIRST { SECTION_T, 0 },
    [ASSIGN_T]        = FIRST { ASSIGN_T, 0 },
    [ALT_T]           = FIRST { ALT_T, 0 },
    [SEMICOLON_T]     = FIRST { SEMICOLON_T, 0 },
    [CHAR_T]          = FIRST { CHAR_T, 0 },
    [STRING_T]        = FIRST { STRING_T, 0 },
    [EMPTY_T]         = FIRST { EMPTY_T, 0 },
    [COMMENT_T]       = FIRST { COMMENT_T, 0 },
    [PARSER_SPEC_NT]  = FIRST { ID_T, SECTION_T, EOF_T,  0 },
    [PATTERN_DEFS_NT] = FIRST { ID_T, SECTION_T, EOF_T, 0 },
    [PATTERN_DEF_NT]  = FIRST { ID_T, 0 },
    [GRAMMAR_NT]      = FIRST { SECTION_T, EOF_T, 0 },
    [RULES_NT]        = FIRST { ID_T, EOF_T, 0  },
    [RULE_NT]         = FIRST { ID_T, 0 },
    [ALTS_NT]         = FIRST { ALT_T, SEMICOLON_T, 0  },
    [ALT_NT]          = FIRST { ID_T, CHAR_T, STRING_T, EMPTY_T, 0 },
    [RHSES_NT]        = FIRST { ID_T, CHAR_T, STRING_T, EMPTY_T, ALT_T, SEMICOLON_T, 0 },
    [RHS_NT]          = FIRST { ID_T, CHAR_T, STRING_T, EMPTY_T, 0 }
};
#undef FIRST

static enum gram_symbol *first_set(enum gram_symbol sym) {
    return first_sets[sym];
}

static union gram_result id_result(char *id) {
    return (union gram_result) { .id = id };
}

static union gram_result lit_result(char *lit) {
    return (union gram_result) { .lit = lit };
}

static union gram_result pdef_result(char *id, char *regex) {
    return (union gram_result) { .pdef = { id, regex } };
}

struct gram_scan_context gram_scan_context(char *input) {
}

struct gram_parse_context gram_parse_context(char *input, void *result_context) {
    struct dfa_context *regex_context = dfa_context(PATTERNS {
        // TODO: eof, alpha, alnum, char, string
        { ID_T,        "id",        "{alpha}{alnum}*" },
        { REGEX_T,     "regex",     "\/[^/\n]*\/"     },
        { SECTION_T,   "section",   "---"             },
        { ASSIGN_T,    "assign",    "="               },
        { ALT_T,       "alt",       "|"               },
        { SEMICOLON_T, "semicolon", ";"               },
        { EMPTY_T,     "empty",     "\$empty"         },
        { COMMENT_T,   "comment",   "\/\/.*\n"        },
        END_PATTERNS
    });

    return (gram_parse_context) {
        .result_context = result_context,
        .scan_context = gram_scan_context(input, regex_context)
    };
}

// bool peek(enum gram_symbol expected, struct gram_parse_context *context);
// bool expect(enum gram_symbol expected, struct gram_parse_context *context);
// void do_action(enum gram_symbol action, union gram_result val, struct gram_parse_context *context);
// union gram_result result(struct gram_parse_context *context);

bool parse_parser_spec(struct gram_parse_context *context) {
    if (parse_pattern_defs(context)) {
        union gram_result pdefs = result(context);

        if (parse_grammar(context) && expect(EOF_T, context)) {
            do_action(DO_PARSER_SPEC, pdefs, context);
            return true;
        }
    }

    return false;
}

bool parse_pattern_defs(struct gram_parse_context *context) {
    if (peek(SECTION_T, context) || peek(EOF_T, context)) {
        return true; // ε
    } else if (parse_pattern_def(context)) {
        while (true) {
            union gram_result last_pdef = result(context);

            if (peek(ID_T, context) && parse_pattern_def(context)) {
                do_action(DO_APPEND_PATTERN_DEF, last_pdef, context);
                continue;
            }

            break;
        }

        return true;
    }

    set_parse_error(PATTERN_DEFS_NT, context);

    return false;
}

bool parse_pattern_def(struct gram_parse_context *context) {
    union gram_result id = id_result(lexeme(context));

    if (expect(ID_T, context)) {
        union gram_result pdef = pdef_result(id.id, lexeme(context));

        if (expect(REGEX_T, context)) {
            do_action(DO_PATTERN_DEF, pdef, context);
            return true;
        }
    }

    return false;
}

bool parse_grammar(struct gram_parse_context *context) {
    if (peek(EOF_T, context)) {
        return true; // ε
    } else if (expect(SECTION_T, context) && parse_rules(context)) {
        return true;
    }

    set_parse_error(GRAMMAR_NT, context);

    return false;
}

bool parse_rules(struct gram_parse_context *context) {
    if (peek(EOF_T, context)) {
        return true; // ε
    } else if (parse_rule(context)) {
        while (true) {
            union gram_result last_rule = result(context);

            if (peek(ID_T, context) && parse_rule(context)) {
                do_action(DO_APPEND_RULE, last_rule, context);
                continue;
            }

            break;
        }

        return true;
    }

    set_parse_error(RULES_NT, context);

    return false;
}

bool parse_rule(struct gram_parse_context *context) {
    union gram_result id = id_result(lexeme(context));

    if (expect(ID_T, context) &&
        expect(ALT_T, context) &&
        parse_alt(context) &&
        parse_alts(context) &&
        expect(SEMICOLON_T, context)) {
        do_action(DO_RULE, id, context);
        return true;
    }

    set_parse_error(RULE_NT, context);

    return false;
}

bool parse_alt(struct gram_parse_context *context) {
    if (parse_rhs(context) && parse_rhses(context)) {
        do_action(DO_ALT, NULL_RESULT, context);
        return true;
    }

    set_parse_error(ALT_NT, context);

    return false;
}

bool parse_alts(struct gram_parse_context *context) {
    if (peek(SEMICOLON_T, context)) {
        return true;  // ε
    } else if (expect(ALT_T, context)) {
        union gram_result last_alt = result(context);

        if (parse_alt(context)) {
            do_action(DO_APPEND_ALT, last_alt, context);
            if (parse_alts(context)) return true;
        }
    }

    set_parse_error(ALTS_NT, context);

    return false;
}

bool parse_rhses(struct gram_parse_context *context) {
    if (peek(ALT_T, context) || peek(SEMICOLON_T, context)) {
        return true; // ε
    } else {
        union gram_result last_rhs = result(context);

        if (parse_rhs(context)) {
            do_action(DO_APPEND_RHS, last_rhs, context);
            if (parse_rhses(context)) return true;
        }
    }

    set_parse_error(RHSES_NT, context);

    return false;
}

bool parse_rhs(struct gram_parse_context *context) {
    if (peek(ID_T, context)) {
        do_action(DO_ID_RHS, id_result(lexeme(context)), context);
        if (expect(ID_T, context)) return true;
    } else if (peek(CHAR_T, context)) {
        do_action(DO_LIT_RHS, lit_result(lexeme(context)), context);
        if (expect(CHAR_T, context)) return true;
    } else if (peek(STRING_T, context)) {
        do_action(DO_LIT_RHS, lit_result(lexeme(context)), context);
        if (expect(STRING_T, context)) return true;
    } else if (peek(EMPTY_T, context)) {
        do_action(DO_EMPTY_RHS, NULL_RESULT, context);
        if (expect(EMPTY_T, context)) return true;
    }

    set_parse_error(RHS_NT, context);

    return false;
}

// void generate_lr_parser(FILE *handle, struct gram_parse_context *context);

