#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdlib.h>
#include <stdio.h>

struct parser_spec {
    struct pattern_def *pattern_defs;
    struct rule *rules;
};

struct pattern_def {
    struct pattern_def *next;
    char *name;
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
    ID,
    LITERAL,
    EMPTY
};

struct rhs {
    struct rhs *next;
    enum rhs_type type;
    char *sym;
};

#endif // GRAM_AST_H_


