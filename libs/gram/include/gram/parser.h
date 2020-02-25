#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <regex/nfa.h>

/*
alpha         /[A-Za-z_]/
alnum         /{alpha}|[0-9]/
id            /{alpha}{alnum}* /
symbol        /{identifier}|{char}|{string}/
regex         //{non-nl}//
comment       /\/\/.*{nl}/

parser_spec       = pattern_defs_head grammar eof;
pattern_defs_head = pattern_def pattern_defs { pattern_defs_head } | $empty;
pattern_defs      = pattern_def { += pattern_def } pattern_defs | $empty;
pattern_def       = id regex { pattern_def };
grammar           = "---" rules_head | $empty;
rules_head        = rule rules { rules_head } | $empty;
rules             = rule { += rule } rules | $empty;
rule              = id '=' alt alts ';' { rule };
alts              = '|' alt { += alt } alts | $empty;
alt               = rhs rhses { alt };
rhses             = rhs { += rhs } rhses | $empty;
rhs               = id { id_rhs(lexeme) }
                  | char { lit_rhs(lexeme) }
                  | string { lit_rhs(lexeme) }
                  | "$empty" { empty };
*/

enum gram_symbol {
    GM_ERROR,

    // terminals
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
    GM_RHS_NT
};

enum gram_error_type {
    GM_SYNTAX_ERROR,
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
    };
};

struct gram_parse_context {
    void *ast;
    struct nfa_context scanner;
    struct nfa_match match;
    enum gram_symbol sym;
    bool has_error;
    struct gram_error error;
};

bool gram_parse_context(struct gram_parse_context *context);
bool gram_parse_has_error(struct gram_parse_context *context);
struct gram_error gram_parser_error(struct gram_parse_context *context);
struct gram_error gram_syntax_error(enum gram_symbol actual, struct regex_loc loc, enum gram_symbol expected);
void gram_print_error(FILE *handle, struct gram_error error);
bool gram_start_scanning(char *input, struct gram_parse_context *context);
enum gram_symbol gram_scan(struct gram_parse_context *context);
enum gram_symbol gram_lookahead(struct gram_parse_context *context);
struct regex_loc gram_location(struct gram_parse_context *context);
bool gram_lexeme(char *lexeme, struct gram_parse_context *context);
bool parse_gram_parser_spec(char *spec, struct gram_parse_context *context);
struct gram_parser_spec *get_gram_parser_spec(struct gram_parse_context *context);

#endif // GRAM_PARSER_H_
