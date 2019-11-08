#ifndef REGEX_DFA_H_
#define REGEX_DFA_H_ 1

#include <stdbool.h>
#include <base/intset.h>
#include <base/array.h>
#include "result_types.h"
#include "parser.h"

enum dfa_pos type {
    SYMBOL_POS,
    EMPTY_POS,
    DOTALL_POS,
    ALT_POS,
    CAT_POS,
    STAR_POS,
    PLUS_POS,
    OPTIONAL_POS
};

// effectively cached computed properties instead of AST nodes
// each node in the AST either has a symbol and associated position,
// or the set of first, last, and follow positions
struct dfa_pos {
    enum dfa_pos_type type;
    unsigned int pos; // unique id, index in the position buffer
    bool nullable;
    union {
        // symbol
        struct { char symbol; };
        // alt, cat, star, plus, optional
        struct { struct intset *firstpos; struct intset *lastpos; struct intset *followpos; };
        // empty, dotall
    };
};

struct dfa_error {
    struct parse_error perror;
};

struct dfa_context {
    struct array *posbuf; // indexed by position
    size_t numpos; // the position index
    struct dfa_pos dfa_pos;
    bool has_error;
    struct dfa_error error;
};

struct dfa_context dfa_context(struct array *posbuf);
void spos(struct dfa_context *context, struct dfa_pos pos);

// dfa position constructors
struct dfa_pos symbol_pos(char symbol);
struct dfa_pos empty_pos();
struct dfa_pos dotall_pos();
struct dfa_pos alt_pos(bool nullable, struct intset *firstpos, struct intset *lastpos, struct intset *followpos);
struct dfa_pos cat_pos(bool nullable, struct intset *firstpos, struct intset *lastpos, struct intset *followpos);
struct dfa_pos star_pos(bool nullable, struct intset *firstpos, struct intset *lastpos, struct intset *followpos);
struct dfa_pos plus_pos(bool nullable, struct intset *firstpos, struct intset *lastpos, struct intset *followpos);
struct dfa_pos optional_pos(bool nullable, struct intset *firstpos, struct intset *lastpos, struct intset *followpos);
struct dfa_pos sub_pos(bool nullable, struct intset *firstpos, struct intset *lastpos, struct intset *followpos);

// parse actions
void noop_dfa(struct dfa_context *context, union rval _);
void do_empty_dfa(struct dfa_context *context, union rval _);
void do_alt_dfa(struct dfa_context *context, union rval lpos);
void do_cat_dfa(struct nfa_context *context, union rval lpos);
void do_dotall_dfa(struct dfa_context *context, union rval _);
void do_symbol_dfa(struct dfa_context *context, union rval sym);
void do_star_dfa(struct dfa_context *context, union rval _);
void do_plus_dfa(struct dfa_context *context, union rval _);
void do_optional_dfa(struct nfa_context *context, union rval _);

void (*dfa_actions[NUMACTIONS])(void *context, union rval lval);

#endif // REGEX_DFA_H_
