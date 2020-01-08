#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <regex/dfa.h>

/*
nl            /\n/
alpha         /[A-Za-z_]/
alnum         /{alpha}|[0-9]/
id            /{alpha}{alnum}* /
char          /'{alnum}+'/
string        /"{alnum}+"/
literal       /({char}|{string})/
symbol        /({identifier}|{literal})/
expression    /\/{non-nl}\//
comment       /\/\/.*{nl}/

---
grammar      = header "---" nl rules eof;
header       = token_defs nl | $empty;
token_defs   = token_def token_defs | $empty;
token_def    = id expression nl;
rules        = rule rules | $empty;
lhs          = id;
rule         = lhs '=' bodies ';';
bodies       = alt alts;
alts         = '|' alt alts | $empty;
alt          = rhs rhses;
rhses        = rhs rhses | $empty;
rhs          = id | literal | "$empty";

*/

enum gram_symbol {
    NOT_REC,

    // terminals
    EOF_T,
    ID_T,
    REGEX_T,
    NEWLINE_T,
    SECTION_T,
    ASSIGN_T,
    ALT_T,
    SEMICOLON_T,
    CHAR_T,
    STRING_T,
    EMPTY_T,
    COMMENT_T,

    // non-terminals
    GRAMMAR_NT,
    HEADER_NT,
    TOKEN_DEFS_NT,
    TOKEN_DEF_NT,
    RULES_NT,
    RULE_NT,
    BODIES_NT,
    ALTS_NT,
    ALT_NT,
    RHSES_NT,
    RHS_NT

    // actions
};

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

struct gram_ast_context {
    void *ast;
};

struct gram_parse_context {
    void *result_context;
    struct gram_scan_context *scan_context;
    bool has_error;
    struct gram_parse_error error;
};

union gram_result {
    void *ast;
};

struct gram_scan_context gram_scan_context(char *input);
struct gram_parse_context gram_parse_context(char *input, void *result_context);
struct gram_parse_error has_parse_error(struct gram_parse_context *context);
bool has_parse_error(struct gram_parse_context *context);
bool peek(enum gram_symbol expected, struct gram_parse_context *context);
bool expect(enum gram_symbol expected, struct gram_parse_context *context);
void do_action(enum gram_symbol action, union gram_result val, struct gram_parse_context *context);
bool parse_grammar(struct gram_parse_context *context);
bool parse_header(struct gram_parse_context *context);
bool parse_token_defs(struct gram_parse_context *context);
bool parse_token_def(struct gram_parse_context *context);
bool parse_rules(struct gram_parse_context *context);
bool parse_lhs(struct gram_parse_context *context);
bool parse_rule(struct gram_parse_context *context);
bool parse_alts(struct gram_parse_context *context);
bool parse_rhses(struct gram_parse_context *context);
bool parse_rhs(struct gram_parse_context *context);
void generate_parser(FILE *handle, struct gram_parse_context *context);

#endif // GRAM_PARSER_H_
