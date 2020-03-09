#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <base/array.h>
#include "gram/parser.h"

struct gram_parser_spec {
    struct gram_pattern_def *pattern_defs;
    struct gram_rule *rules;
};

struct gram_pattern_def {
    char *id;
    char *regex;
    struct gram_pattern_def *next;
};

struct gram_rule {
    char *id;
    struct gram_alt *alts;
    struct gram_rule *next;
};

struct gram_alt {
    struct gram_rhs *rhses;
    struct gram_alt *next;
};

enum gram_rhs_type {
    GM_ID_RHS,
    GM_CHAR_RHS,
    GM_STRING_RHS,
    GM_EMPTY_RHS
};

struct gram_rhs {
    enum gram_rhs_type type;
    union {
        char *sym;
    };
    struct gram_rhs *next;
};

struct gram_ast_context {
    void *ast;
    struct array *allocs;
};

bool gram_ast_context(struct gram_ast_context *context);
void free_gram_ast_context(struct gram_ast_context *context);
struct gram_parser_spec *gram_parser_spec(struct gram_ast_context *context);

extern struct gram_result_interface gram_ast_iface;

#endif // GRAM_AST_H_


