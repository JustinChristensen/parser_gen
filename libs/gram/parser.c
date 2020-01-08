#include <stdio.h>
#include <stdlib.h>
#include "gram/parser.h"
#include "regex/dfa.h"

struct gram_scan_context gram_scan_context(char *input) {
}

struct gram_parse_context gram_parse_context(char *input, void *result_context) {
    struct dfa_context *regex_context = dfa_context(PATTERNS {
        // TODO: eof, alpha, alnum, char, string
        { ID_T, "id", "{alpha}{alnum}*" },
        { REGEX_T, NULL, "\/[^/]*\/" },
        { SECTION_T, NULL, "---" },
        { ASSIGN_T, NULL, "=" },
        { ALT_T, NULL, "|" },
        { SEMICOLON_T, NULL, ";" },
        { EMPTY_T, NULL, "$empty" },
        { COMMENT_T, NULL, "\/\/.*\n" },
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

bool parse_grammar(struct gram_parse_context *context) {
    if (parse_header(context) &&
        expect(SECTION_T, context) &&
        expect(NEWLINE_T, context) &&
        parse_rules(context)) {
        return true;
    }

    return false;
}

bool parse_header(struct gram_parse_context *context);
bool parse_token_defs(struct gram_parse_context *context);
bool parse_token_def(struct gram_parse_context *context);
bool parse_rules(struct gram_parse_context *context);
bool parse_lhs(struct gram_parse_context *context);
bool parse_rule(struct gram_parse_context *context);
bool parse_alts(struct gram_parse_context *context);
bool parse_rhses(struct gram_parse_context *context);
bool parse_rhs(struct gram_parse_context *context);

// void generate_parser(FILE *handle, struct gram_parse_context *context);

