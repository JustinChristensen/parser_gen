#ifndef REGEX_RESULT_TYPES_H_
#define REGEX_RESULT_TYPES_H_ 1

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

union rval {
    void *_;
    struct expr *expr;
    struct nfa mach;
    char sym;
};

static const union rval NULLRVAL = { NULL };

#endif // REGEX_RESULT_TYPES_H_

