#ifndef REGEX_DFA_H_
#define REGEX_DFA_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include "base.h"

struct dfa_context {
    struct regex_pattern const *patterns
};

struct dfa_match {
    struct dfa_context *context;
    char *input;
    struct regex_loc input_loc;
    char *match_start;
    struct regex_loc match_loc;
};

struct dfa_context dfa_context(struct regex_pattern const **patterns);
void dfa_regex(int sym, char *tag, char *pattern, struct dfa_context *context);
struct dfa_match dfa_match_state(char *input, struct regex_loc loc, struct dfa_context *context);
int dfa_match(struct dfa_match *match);
struct regex_loc dfa_match_loc(struct dfa_match *match);
void dfa_match_lexeme(char *lexeme, struct dfa_match *match);

#endif // REGEX_DFA_H_
