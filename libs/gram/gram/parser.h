#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>

/*
nl            \n
alpha         [A-Za-z_]
alnum         {alpha}|[0-9]
identifier    {alpha}{alnum}*
literal       ('{alnum}+'|"{alnum}+")
symbol        ({identifier}|{literal})
expression    {non-nl}

===

grammar     = header rules eof;
header      = token_defs nl "===" nl nl | ε;
token_defs  = token_def token_defs
token_def   = identifier expression nl
rules       = rule rules | ε;
lhs         = identifier;
rule        = lhs '=' alts ';';
alts        = rhses alts_tail;
alts_tail   = '|' rhses alts_tail | ε;
rhses       = rhs rhses_tail;
rhses_tail  = rhs rhses_tail | ε;
rhs         = symbol | "$empty";
*/

#endif // GRAM_PARSER_H_
