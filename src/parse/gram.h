#ifndef PARSE_GRAM_H_
#define PARSE_GRAM_H_ 1

#include <stdbool.h>
#include <regex/dfa.h>

/*
grammar = rules ;           { grammar(rules) }
rules = rule rules
      | ε ;
rule = lhs '=' alts ';' ;   { rule(lhs, alts) }
alts = alts '|' rhses       { alts(alts, rhses) }
     | rhses ;
rhses = rhses symbol        { rhses(rhses, symbol) }
      | ε ;
lhs = nonterminal ;
symbol = terminal | nonterminal ;
*/

enum token_type {
    T_PIPE,
    T_EQUAL,
    T_SEMICOLON,
    T_TERMINAL,
    T_NONTERMINAL
};

struct location {
    int line;
    int col;
};

struct token {
    enum token_type type;
    char *lexeme;
    struct location;
};

struct parse_error {
};

struct gram_context {
    char *input;
    struct dfa_context *dfa_context;
    struct token lookahead;
    bool has_error;
    struct parse_error error;
};

struct gram_context gram_context(char *input, struct dfa_context *dfa_context);
bool parse_grammar(struct gram_context *context);

#endif // PARSE_GRAM_H_
