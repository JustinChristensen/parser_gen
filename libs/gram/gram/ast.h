#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdlib.h>
#include <stdio.h>

struct grammar {
    struct token_def *token_defs;
    struct rule *rules;
};

struct token_def {
    struct token_def *next;
    char *id;
    char *regex;
};

struct rule {
    struct rule *next;
    char *id;
    struct alt *alts;
};

struct alt {
    struct alt *next;
    struct rhs *rhses;
};

enum rhs_type {
    TERM,
    NONTERM,
    EMPTY
};

struct rhs {
    struct rhs *next;
    enum rhs_type type;
    char *sym;
};

#endif // GRAM_AST_H_


