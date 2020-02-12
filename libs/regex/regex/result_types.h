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
    ID_EXPR,
    RANGE_EXPR,
    CHAR_CLASS_EXPR,
    NEG_CLASS_EXPR,
    SYMBOL_EXPR,
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
        // sym
        char symbol;
        // id
        char *id;
        // empty, dotall
    };
};

enum nfa_state_type {
    ACCEPTING_STATE,
    EPSILON_STATE,
    BRANCH_STATE,
    CLASS_STATE,
    SYMBOL_STATE,
    DOTALL_STATE
};

struct nfa_state {
    enum nfa_state_type type;
    int id;
    union {
        // epsilon, dotall, symbol, class
        struct {
            struct nfa_state *next;
            union {
                char symbol;
                bool *char_class;
            };
        };
        // branch
        struct { struct nfa_state *left; struct nfa_state *right; };
        // accepting
    };
};

struct nfa {
    struct nfa_state *start;
    struct nfa_state **end;
    struct nfa_state **end1;
};

enum dfa_node_type {
    SYMBOL_NODE,
    EMPTY_NODE,
    DOTALL_NODE,
    ALT_NODE,
    CAT_NODE,
    STAR_NODE,
    PLUS_NODE,
    OPTIONAL_NODE
};

// cached computed properties as described in section 3.9 of
// the compiler's book
struct dfa_pos {
    bool nullable;
    struct intset *firstpos;
    struct intset *lastpos;
    struct intset *followpos;
};

// ast node
struct dfa_node {
    enum dfa_node_type type;
    unsigned int id; // unique id, index in the node buffer
    union {
        // symbol
        struct { char symbol; };
        // alt, cat
        struct { struct dfa_node *left; struct dfa_node *right; };
        // star, plus, optional
        struct { struct dfa_node *node; };
        // empty, dotall
    };
    struct dfa_pos pos; // computed position properties
};

union regex_token_val {
    char sym;
    int num;
    struct char_range range;
};

union regex_result {
    char *id;
    union regex_token_val tval;
    struct expr *expr;
    struct nfa mach;
};

static const union regex_result NULLRVAL = { 0 };

#endif // REGEX_RESULT_TYPES_H_

