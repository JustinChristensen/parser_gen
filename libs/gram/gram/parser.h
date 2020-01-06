#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>

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
===

grammar      = header rules eof;
header       = token_defs nl "===" nl nl | $empty;
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
    UNRECOGNIZED,

    // terminals
    ID_T,
    REGEX_T,
    NL_T,
    SECTION_SEP_T,
    ASSIGN_T,
    ALT_T,
    SEMICOLON_T,
    CHAR_T,
    STRING_T,
    EMPTY_T,

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

struct gram_scan_context {
};

struct gram_parse_error {
};

struct gram_parse_context {
    void *ast;
    bool has_error;
    struct gram_parse_error error;
};

struct gram_scan_context();
struct gram_parse_context();
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
void write_parser(FILE *handle, struct gram_parse_context *context);

#endif // GRAM_PARSER_H_
