#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <regex/dfa.h>

/*
alpha         /[A-Za-z_]/
alnum         /{alpha}|[0-9]/
id            /{alpha}{alnum}* /
char          /'{alnum}+'/
string        /"{alnum}+"/
symbol        /({identifier}|{literal})/
regex         /\/{non-nl}\//
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
    NIL,

    // terminals
    EOF_T,
    ID_T,
    REGEX_T,
    SECTION_T,
    ASSIGN_T,
    ALT_T,
    SEMICOLON_T,
    CHAR_T,
    STRING_T,
    EMPTY_T,
    COMMENT_T,

    // non-terminals
    PARSER_SPEC_NT,
    PATTERN_DEFS_HEAD_NT,
    PATTERN_DEFS_NT,
    PATTERN_DEF_NT,
    GRAMMAR_NT,
    RULES_HEAD_NT,
    RULES_NT,
    RULE_NT,
    ALTS_HEAD_NT,
    ALTS_NT,
    ALT_NT,
    RHSES_NT,
    RHS_NT,

    // actions
    DO_PARSER_SPEC,
    DO_PATTERN_DEF,
    DO_APPEND_PATTERN_DEF,
    DO_PATTERN_DEFS_HEAD,
    DO_RULE,
    DO_APPEND_RULE,
    DO_RULES_HEAD,
    DO_ALT,
    DO_APPEND_ALT,
    DO_ALTS_HEAD,
    DO_ID_RHS,
    DO_LIT_RHS,
    DO_EMPTY_RHS,
    DO_APPEND_RHS,
    DO_RHSES_HEAD
};

#define NUM_TERMINALS (COMMENT_T + 1)
#define NUM_NONTERMINALS (RHS_NT + 1 - NUM_TERMINALS)

struct gram_loc {
    int line;
    int col;
};

struct gram_token {
    enum gram_symbol type;
    struct gram_loc loc;
    char *lexeme;
    size_t lxlen;
};

struct gram_scan_context {
    struct dfa_context *re_context;
    char *input;
    struct gram_loc input_loc;
    struct gram_token token;
};

struct gram_parse_error {
    struct gram_loc loc;
    enum gram_symbol actual;
    enum gram_symbol *expected;
};

#define GRAM_PARSE_ERROR_FMT_START "| Parse Error\n|\n| Got: %s\n| Expected: "
#define GRAM_PARSE_ERROR_FMT_LOC "\n|\n| At: "
#define GRAM_PARSE_ERROR_FMT_END "\n|\n"

union gram_result {
    void *ast;
    char *id;
    char *lit;
    struct {
        char *id;
        char *regex;
    } pdef;
};

static const union gram_result NULL_RESULT = { NULL };

struct gram_parse_context {
    void *result_context;
    void (**actions)(union gram_result result, void *result_context);
    union gram_result (*result)(void *result_context);
    struct gram_scan_context scan_context;
    struct gram_token token;
    bool has_error;
    struct gram_parse_error error;
};

struct gram_scan_context gram_scan_context(char *input, struct dfa_context *re_context);
struct gram_scan_context set_input(char *input, struct gram_scan_context context);
struct gram_scan_context scan(struct gram_scan_context context);
struct gram_token token(struct gram_scan_context context);
struct gram_parse_context gram_parse_context(
    void *result_context,
    void (**actions)(union gram_result result, void *result_context),
    union gram_result (*result)(void *result_context)
);
struct gram_parse_error gram_parse_error();
void set_parse_error(enum gram_symbol expected, struct gram_parse_context *context);
bool has_parse_error(struct gram_parse_context *context);
void print_parse_error(struct gram_parse_error *error);
enum gram_symbol lookahead(struct gram_parse_context *context);
struct gram_loc location(struct gram_parse_context *context);
char *lexeme(struct gram_parse_context *context);
bool peek(enum gram_symbol expected, struct gram_parse_context *context);
bool expect(enum gram_symbol expected, struct gram_parse_context *context);
void do_action(enum gram_symbol action, union gram_result val, struct gram_parse_context *context);
union gram_result result(struct gram_parse_context *context);
void start_scanning(char *input, struct gram_parse_context *context);
bool parse_parser_spec(char *input, struct gram_parse_context *context);
bool parse_pattern_defs_head(struct gram_parse_context *context);
bool parse_pattern_defs(struct gram_parse_context *context);
bool parse_pattern_def(struct gram_parse_context *context);
bool parse_grammar(struct gram_parse_context *context);
bool parse_rules_head(struct gram_parse_context *context);
bool parse_rules(struct gram_parse_context *context);
bool parse_rule(struct gram_parse_context *context);
bool parse_alts_head(struct gram_parse_context *context);
bool parse_alts(struct gram_parse_context *context);
bool parse_alt(struct gram_parse_context *context);
bool parse_rhses(struct gram_parse_context *context);
bool parse_rhs(struct gram_parse_context *context);
void generate_lr_parser(FILE *handle, struct gram_parse_context *context);

#endif // GRAM_PARSER_H_
