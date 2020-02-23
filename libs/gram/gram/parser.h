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
rule              = id '=' alts_head ';' { rule };
alts_head         = alt alts { alts_head };
alts              = '|' alt { += alt } alts | $empty;
alt               = rhs rhses { rhses_head } { alt };
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
    GM_PATTERN_DEFS_HEAD_NT,
    GM_PATTERN_DEFS_NT,
    GM_PATTERN_DEF_NT,
    GM_GRAMMAR_NT,
    GM_RULES_HEAD_NT,
    GM_RULES_NT,
    GM_RULE_NT,
    GM_ALTS_HEAD_NT,
    GM_ALTS_NT,
    GM_ALT_NT,
    GM_RHSES_NT,
    GM_RHS_NT,

    // actions
    GM_DO_PARSER_SPEC,
    GM_DO_PATTERN_DEF,
    GM_DO_APPEND_PATTERN_DEF,
    GM_DO_PATTERN_DEFS_HEAD,
    GM_DO_RULE,
    GM_DO_APPEND_RULE,
    GM_DO_RULES_HEAD,
    GM_DO_ALT,
    GM_DO_APPEND_ALT,
    GM_DO_ALTS_HEAD,
    GM_DO_ID_RHS,
    GM_DO_LIT_RHS,
    GM_DO_EMPTY_RHS,
    GM_DO_APPEND_RHS,
    GM_DO_RHSES_HEAD
};

#define GM_NUM_TERMINALS (GM_WHITESPACE_T + 1)
#define GM_NUM_SYMBOLS (GM_RHS_NT + 1)
#define GM_NUM_NONTERMINALS (GM_NUM_SYMBOLS - GM_NUM_TERMINALS)
#define GM_NUM_ACTIONS ((GM_DO_RHSES_HEAD + 1) - GM_NUM_NONTERMINALS)
#define GM_AI(sym) (sym - GM_NUM_NONTERMINALS)

enum gram_error_type {
    GM_SYNTAX_ERROR
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

#define GM_SYNTAX_ERROR_FMT_START "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define GM_SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
#define GM_SYNTAX_ERROR_FMT_END "\n|\n"

union gram_result {
    void *ast;
    char *id;
    char *lit;
    struct {
        char *id;
        char *regex;
    } pdef;
};

static const union gram_result GM_NULL_RESULT = { NULL };

struct gram_parse_context {
    void *result;
    bool (**actions)(union gram_result val, void *result);
    union gram_result (*get_result)(void *result);
    struct nfa_context scanner;
    struct nfa_match match;
    enum gram_symbol sym;
    bool has_error;
    struct gram_error error;
};

bool gm_parse_context(
    struct gram_parse_context *context,
    void *result,
    void (**actions)(union gram_result val, void *result),
    union gram_result (*get_result)(void *result)
);
bool gm_parser_has_error(struct gram_parse_context *context);
struct gram_error gm_parser_error(struct gram_parse_context *context);
struct gram_error gm_syntax_error(enum gram_symbol actual, struct regex_loc loc, enum gram_symbol expected);
void gm_print_error(struct gram_error error);
bool gm_start_scanning(char *input, struct gram_parse_context *context);
enum gram_symbol gm_scan(struct gram_parse_context *context);
enum gram_symbol gm_lookahead(struct gram_parse_context *context);
struct regex_loc gm_location(struct gram_parse_context *context);
char *gm_lexeme(struct gram_parse_context *context);
bool gm_parse_parser_spec(char *input, struct gram_parse_context *context);

#endif // GRAM_PARSER_H_
