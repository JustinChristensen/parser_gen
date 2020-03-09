#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <base/array.h>
#include <base/hash_table.h>
#include <regex/nfa.h>

/*
parser_spec       = pattern_defs_head grammar eof;
pattern_defs_head = pattern_def pattern_defs { pattern_defs_head } | $empty;
pattern_defs      = pattern_def { += pattern_def } pattern_defs | $empty;
pdef_flags        = '@' | '-' | $empty;
pattern_def       = '@' id regex { tag_only_pattern_def }
                  | '-' id regex { skip_pattern_def }
                  | pdef_flags id regex { pattern_def };
grammar           = "---" rules_head | $empty;
rules_head        = rule rules { rules_head } | $empty;
rules             = rule { += rule } rules | $empty;
rule              = id '=' alt alts ';' { rule };
alts              = '|' alt { += alt } alts | $empty;
alt               = rhs rhses { alt };
rhses             = rhs { += rhs } rhses | $empty;
rhs               = id { id_rhs(lexeme) }
                  | char { char_rhs(lexeme) }
                  | string { string_rhs(lexeme) }
                  | "$empty" { empty };
*/

enum gram_symbol {
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
    GM_COMMENT_T,
    GM_ID_T,
    GM_WHITESPACE_T,

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
    GM_RHS_NT,

    // actions
    GM_DO_PARSER_SPEC,
    GM_DO_PATTERN_DEFS_HEAD,
    GM_DO_APPEND_PATTERN_DEF,
    GM_DO_PATTERN_DEF,
    GM_DO_RULES_HEAD,
    GM_DO_APPEND_RULE,
    GM_DO_RULE,
    GM_DO_ALTS_HEAD,
    GM_DO_APPEND_ALT,
    GM_DO_ALT,
    GM_DO_APPEND_RHS,
    GM_DO_ID_RHS,
    GM_DO_CHAR_RHS,
    GM_DO_STRING_RHS,
    GM_DO_EMPTY_RHS
};

enum gram_error_type {
    GM_SYNTAX_ERROR,
    GM_SCANNER_ERROR,
    GM_OOM_ERROR
};

struct gram_error {
    enum gram_error_type type;
    union {
        struct {
            enum gram_symbol actual;
            struct regex_loc loc;
            enum gram_symbol *expected;
        };
        struct regex_error scanerr;
    };
};

union gram_result {
    void *ast;
    char *id;
    char *lit;
    struct {
        char *id;
        char *regex;
        struct {
            uint8_t tag_only : 1;
            uint8_t skip : 1;
        };
    } pdef;
};

struct gram_parse_context {
    void *result;
    struct gram_result_interface const *ri;
    struct nfa_context scanner;
    struct nfa_match match;
    enum gram_symbol sym;
    bool has_error;
    struct gram_error error;
};

struct gram_result_interface {
    bool (*const *actions)(union gram_result val, struct gram_parse_context *context);
    union gram_result (*result)(struct gram_parse_context *context);
};

bool gram_parse_context(struct gram_parse_context *context, void *result, struct gram_result_interface const *const ri);
void free_gram_parse_context(struct gram_parse_context *context);
bool gram_parse_has_error(struct gram_parse_context *context);
struct gram_error gram_parser_error(struct gram_parse_context *context);
struct gram_error gram_syntax_error(enum gram_symbol actual, struct regex_loc loc, enum gram_symbol expected);
void print_gram_error(FILE *handle, struct gram_error error);
bool gram_start_scanning(char *input, struct gram_parse_context *context);
enum gram_symbol gram_scan(struct gram_parse_context *context);
enum gram_symbol gram_lookahead(struct gram_parse_context *context);
struct regex_loc gram_location(struct gram_parse_context *context);
bool gram_lexeme(char *lexeme, struct gram_parse_context *context);
bool parse_gram_parser_spec(char *spec, struct gram_parse_context *context);
struct gram_parser_spec *get_gram_parser_spec(struct gram_parse_context *context);
void print_gram_tokens(FILE *handle, char *spec);

#endif // GRAM_PARSER_H_
