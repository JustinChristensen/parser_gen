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

---
parser_spec  = pattern_defs grammar eof;
pattern_defs = pattern_def { += pattern_def } pattern_defs | $empty;
pattern_def  = id regex { pattern_def } ;
grammar      = "---" rules | $empty;
rules        = rule { += rule } rules | $empty;
rule         = id '=' alt alts ';' { rule };
alts         = '|' alt { += alt } alts | $empty;
alt          = rhs rhses { alt };
rhses        = rhs { += rhs } rhses | $empty;
rhs          = id { id_rhs(lexeme) }
             | char { lit_rhs(lexeme) }
             | string { lit_rhs(lexeme) }
             | "$empty" { empty };

Note:
parse_pattern_defs and parse_rules could be made recursive by making a separate routine
    for the head of the list that doesn't try to append the (non-existant) previous term, or
    by ensuring that pattern_def precedeces any place where pattern_defs is used in the above grammar

parse_alts and parse_rhses could be made iterative, and that might reduce the need for checking
    if the current token is in the follow set, i.e. ALT_T, SEMICOLON_T

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
    PATTERN_DEFS_NT,
    PATTERN_DEF_NT,
    GRAMMAR_NT,
    RULES_NT,
    RULE_NT,
    ALTS_NT,
    ALT_NT,
    RHSES_NT,
    RHS_NT,

    // actions
    DO_PARSER_SPEC,
    DO_APPEND_PATTERN_DEF,
    DO_PATTERN_DEF,
    DO_APPEND_RULE,
    DO_RULE,
    DO_ALT,
    DO_APPEND_ALT,
    DO_APPEND_RHS,
    DO_ID_RHS,
    DO_LIT_RHS,
    DO_EMPTY_RHS
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
};

struct gram_scan_context {
    char *input;
    struct dfa_context *rec;
    struct gram_loc loc;
    struct gram_token token;
};

struct gram_parse_error {
    struct gram_loc loc;
    enum gram_symbol actual;
    enum gram_symbol *expected;
};

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
    struct gram_scan_context *scan_context;
    bool has_error;
    struct gram_parse_error error;
};

struct gram_scan_context gram_scan_context(char *input);
struct gram_parse_context gram_parse_context(
    char *input,
    void *result_context,
    void (**actions)(union gram_result result, void *result_context)
);
struct gram_parse_error gram_parse_error();
void set_parse_error(enum gram_symbol expected, struct gram_parse_context *context);
bool has_parse_error(struct gram_parse_context *context);
bool peek(enum gram_symbol expected, struct gram_parse_context *context);
bool expect(enum gram_symbol expected, struct gram_parse_context *context);
void do_action(enum gram_symbol action, union gram_result val, struct gram_parse_context *context);
union gram_result result(struct gram_parse_context *context);
bool parse_parser_spec(struct gram_parse_context *context);
bool parse_pattern_defs(struct gram_parse_context *context);
bool parse_pattern_def(struct gram_parse_context *context);
bool parse_grammar(struct gram_parse_context *context);
bool parse_rules(struct gram_parse_context *context);
bool parse_rule(struct gram_parse_context *context);
bool parse_bodies(struct gram_parse_context *context);
bool parse_alt(struct gram_parse_context *context);
bool parse_alts(struct gram_parse_context *context);
bool parse_rhses(struct gram_parse_context *context);
bool parse_rhs(struct gram_parse_context *context);
void generate_lr_parser(FILE *handle, struct gram_parse_context *context);

#endif // GRAM_PARSER_H_
