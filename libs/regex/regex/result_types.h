#ifndef REGEX_RESULT_TYPES_H_
#define REGEX_RESULT_TYPES_H_ 1

#include <stdlib.h>
#include <stdbool.h>

enum expr_type {
    NULL_EXPR,
    EMPTY_EXPR,
    DOTALL_EXPR,
    ALT_EXPR,
    CAT_EXPR,
    STAR_EXPR,
    PLUS_EXPR,
    OPTIONAL_EXPR,
    SUB_EXPR,
    SYMBOL_EXPR
};

struct expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct expr *lexpr; struct expr *rexpr; };
        // star, plus, optional, sub
        struct { struct expr *expr; };
        // sym
        struct { char symbol; };
        // empty, dotall
    };
};

enum nfa_state_type {
    ACCEPTING_STATE,
    EPSILON_STATE,
    BRANCH_STATE,
    SYMBOL_STATE,
    DOTALL_STATE
};

struct nfa_state {
    enum nfa_state_type type;
    int id;
    union {
        // epsilon, dotall, symbol
        struct { struct nfa_state *next; char symbol; };
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


union rval {
    struct expr *expr;
    struct nfa mach;
    struct dfa_node node;
    char sym;
};

static const union rval NULLRVAL = { NULL };

#endif // REGEX_RESULT_TYPES_H_

