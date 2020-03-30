#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <base/array.h>
#include <base/hash_table.h>
#include <regex/nfa.h>
#include "gram/spec.h"

/*
parser_spec       = pattern_defs grammar eof;
pattern_defs      = pattern_def pattern_defs { += pattern_def } { pattern_defs_head } | $empty;
pdef_flags        = '@' | '-' | $empty;
pattern_def       = pdef_flags id regex { pattern_def };
grammar           = "---" rules | $empty;
rules             = rule rules { += rule } { rules_head } | $empty;
rule              = id '=' alt alts ';' { rule };
alts              = '|' alt { += alt } alts | $empty;
alt               = rhs rhses { alt };
rhses             = rhs { += rhs } rhses | $empty;
rhs               = id { id_rhs(lexeme) }
                  | char { char_rhs(lexeme) }
                  | string { string_rhs(lexeme) }
                  | "$empty" { empty };
*/

enum gram_parser_symbol {
    GM_ERROR,

    // terminals
    GM_EOF_T,
    GM_TAG_ONLY_T,
    GM_SKIP_T,
    GM_REGEX_T,
    GM_SECTION_T,
    GM_ASSIGN_T,
    GM_ALT_T,
    GM_SEMICOLON_T,
    GM_CHAR_T,
    GM_STRING_T,
    GM_EMPTY_T,
    GM_END_T,
    GM_ID_T,

    // non-terminals
    GM_PARSER_SPEC_NT,
    GM_PATTERN_DEFS_NT,
    GM_PATTERN_DEF_NT,
    GM_GRAMMAR_NT,
    GM_RULES_NT,
    GM_RULE_NT,
    GM_ALTS_NT,
    GM_ALT_NT,
    GM_RHSES_NT,
    GM_RHS_NT
};

enum gram_parse_error_type {
    GM_PARSE_SYNTAX_ERROR,
    GM_PARSE_OOM_ERROR,
    GM_PARSE_SCANNER_ERROR
};

struct gram_parse_error {
    enum gram_parse_error_type type;
    union {
        struct {
            enum gram_parser_symbol actual;
            struct regex_loc loc;
            enum gram_parser_symbol *expected;
        };
        struct { char *file; int col; };
        struct regex_error scanerr;
    };
};

struct gram_parse_context {
    struct gram_stats stats;
    struct hash_table *symtab;

    struct gram_symbol *current_rule;
    struct nfa_context scanner;
    struct nfa_match match;
    enum gram_parser_symbol sym;
};

bool gram_parse_context(struct gram_parse_error *error, struct gram_parse_context *context);
bool gram_start_scanning(struct gram_parse_error *error, char *input, struct gram_parse_context *context);
enum gram_parser_symbol gram_scan(struct gram_parse_context *context);
enum gram_parser_symbol gram_lookahead(struct gram_parse_context *context);
struct regex_loc gram_location(struct gram_parse_context *context);
bool gram_lexeme(char *lexeme, struct gram_parse_context *context);
bool gram_parse(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    char *input, struct gram_parse_context *context
);
struct hash_table *gram_symtab(struct gram_parse_context const *context);
void free_gram_parse_context(struct gram_parse_context *context);
void print_gram_parse_error(FILE *handle, struct gram_parse_error error);
void print_gram_tokens(FILE *handle, char *spec);

#endif // GRAM_PARSER_H_
