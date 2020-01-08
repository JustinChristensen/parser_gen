#ifndef REGEX_DFA_H_
#define REGEX_DFA_H_ 1

#include <stdlib.h>
#include <stdio.h>

struct dfa_context {
};

struct regex_pattern {
    int sym;
    char *name;
    char *pattern;
};

struct match_state {
    char *input;
    char *match;
    struct {
        int line;
        int column;
    } loc;
};

void dfa_regex(int sym, char *name, char *pattern, struct dfa_context *context);
int dfa_match(struct dfa_context *context);

#endif // REGEX_DFA_H_
