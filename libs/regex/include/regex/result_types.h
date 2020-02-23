#ifndef REGEX_RESULT_TYPES_H_
#define REGEX_RESULT_TYPES_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <regex/base.h>

enum expr_type {
    RX_NULL_EXPR,
    RX_EMPTY_EXPR,
    RX_DOTALL_EXPR,
    RX_ALT_EXPR,
    RX_CAT_EXPR,
    RX_SUB_EXPR,
    RX_TAG_EXPR,
    RX_RANGE_EXPR,
    RX_CHAR_CLASS_EXPR,
    RX_NEG_CLASS_EXPR,
    RX_CHAR_EXPR,
    RX_STAR_EXPR,
    RX_PLUS_EXPR,
    RX_OPTIONAL_EXPR,
    RX_REPEAT_EXACT_EXPR
};

struct regex_expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct regex_expr *lexpr; struct regex_expr *rexpr; };
        // star, plus, optional, sub, repeat_exact, range
        struct {
            struct regex_expr *expr;
            union {
                int num;
                struct regex_char_range range;
            };
        };
        // char_class, neg_class
        struct regex_expr *ranges;
        // char
        char ch;
        // tag
        char *tag;
        // empty, dotall
    };
};

enum nfa_state_type {
    RX_ACCEPTING_STATE,
    RX_EPSILON_STATE,
    RX_BRANCH_STATE,
    RX_CHAR_STATE,
    RX_CLASS_STATE,
    RX_DOTALL_STATE
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
    struct regex_expr *expr;
    struct nfa mach;
};

static union regex_result const RX_NULL_RESULT = { 0 };

#endif // REGEX_RESULT_TYPES_H_


