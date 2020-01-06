#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>

/*
nl            \n
alpha         [A-Za-z_]
alnum         {alpha}|[0-9]
identifier    {alpha}{alnum}*
char          '{alnum}+'
string        "{alnum}+"
literal       ({char}|{string})
symbol        ({identifier}|{literal})
expression    {non-nl}

===

grammar         = header rules eof;
header          = token_defs nl "===" nl nl | $empty;
token_defs      = token_def token_defs_tail;
token_defs_tail = token_def token_defs_tail | $empty;
token_def       = identifier expression nl;
rules           = rule rules | $empty;
lhs             = identifier;
rule            = lhs '=' alts ';';
alts            = rhses alts_tail;
alts_tail       = '|' rhses alts_tail | $empty;
rhses           = rhs rhses_tail;
rhses_tail      = rhs rhses_tail | $empty;
rhs             = symbol | "$empty";
*/

enum gram_symbol {
};

struct gram_scan_context {
};

struct gram_parse_error {
};

struct gram_parse_context {
    void *ast;
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
