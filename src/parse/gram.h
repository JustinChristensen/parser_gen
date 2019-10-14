#ifndef PARSE_GRAM_H_
#define PARSE_GRAM_H_ 1

#include <stdbool.h>

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

struct gram_context {
};

struct gram_context gram_context(char *grammar);
bool parse_grammar(struct gram_context *context);

#endif // PARSE_GRAM_H_
