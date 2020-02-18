#ifndef REGEX_RESULT_TYPES_H_
#define REGEX_RESULT_TYPES_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base.h"

enum expr_type {
    NULL_EXPR,
    EMPTY_EXPR,
    DOTALL_EXPR,
    ALT_EXPR,
    CAT_EXPR,
    SUB_EXPR,
    TAG_EXPR,
    RANGE_EXPR,
    CHAR_CLASS_EXPR,
    NEG_CLASS_EXPR,
    CHAR_EXPR,
    STAR_EXPR,
    PLUS_EXPR,
    OPTIONAL_EXPR,
    REPEAT_EXACT_EXPR
};

struct expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct expr *lexpr; struct expr *rexpr; };
        // star, plus, optional, sub, repeat_exact, range
        struct {
            struct expr *expr;
            union {
                int num;
                struct char_range range;
            };
        };
        // char_class, neg_class
        struct expr *ranges;
        // char
        char ch;
        // tag
        char *tag;
        // empty, dotall
    };
};

enum nfa_state_type {
    ACCEPTING_STATE,
    EPSILON_STATE,
    BRANCH_STATE,
    CHAR_STATE,
    CLASS_STATE,
    DOTALL_STATE
};

struct nfa_state {
    enum nfa_state_type type;
    int id;
    union {
        // epsilon, dotall, char, class
        struct {
            struct nfa_state *next;
            union {
                char ch;
                bool *char_class;
            };
        };
        // branch
        struct { struct nfa_state *left; struct nfa_state *right; };
        // accepting
        struct { int sym; };
    };
};

struct nfa {
    struct nfa_state *start;
    struct nfa_state **end;
    struct nfa_state **end1;
};

union regex_token_val {
    char ch;
    int num;
    struct char_range range;
};

union regex_result {
    char *tag;
    union regex_token_val tval;
    struct expr *expr;
    struct nfa mach;
};

static const union regex_result NULLRVAL = { 0 };

#endif // REGEX_RESULT_TYPES_H_

