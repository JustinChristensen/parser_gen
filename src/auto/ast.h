#ifndef AUTO_AST_H_
#define AUTO_AST_H_ 1

enum expr_type {
    ALT,
    CAT,
    STAR,
    SUBEXPR,
    SYMBOL,
    EMPTY
};

struct expr {
    enum expr_type type;
    union {
        // alt, cat
        struct { struct expr lexpr; struct expr rexpr; };
        // star, sub
        struct { struct expr expr; };
        // sym
        struct { char symbol; };
        // empty
    };
};

struct expr alt_expr(struct expr lexpr, struct expr rexpr);
struct expr cat_expr(struct expr lexpr, struct expr rexpr);
struct expr star_expr(struct expr expr);
struct expr sub_expr(struct expr expr);
struct expr symbol_expr(char symbol);
struct expr empty_expr();

#endif // AUTO_AST_H_

