#ifndef BASE_rbtree_H_
#define BASE_rbtree_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/array.h"

enum rb_trav_order {
    RB_PRE,
    RB_IN,
    RB_POST
};

struct rb_assoc {
    void *key;
    void *val;
};

struct rb_node {
    struct rb_assoc assoc;
    bool red;
    struct rb_node *left;
    struct rb_node *right;
};

struct rb_iter {
    struct array *stack;
    struct rb_node *root;
    struct rb_node *last;
    enum rb_trav_order order;
};

struct rb_assoc rb_assoc(void *key, void *val);
struct rb_assoc rb_node_assoc(struct rb_node const *node);
void *rbkey(struct rb_node const *node);
void *rbval(struct rb_node const *node);
void rb_iter(enum rb_trav_order order, struct rb_node const *node, struct rb_iter *it);
void free_rb_iter(struct rb_iter *it);
struct rb_node *rbnext(struct rb_iter *it);
struct rb_node *rbmin(struct rb_node *node);
struct rb_node *rbmax(struct rb_node *node);
struct rb_node *rbfind(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct rb_node const *node
);
bool rbcontains(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct rb_node const *node
);
struct rb_node *rbinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct rb_node *node
);
bool rbtree_eq(
    bool (*keyeq) (void const *a, void const *b),
    bool (*valeq) (void const *a, void const *b),
    struct rb_node const *s,
    struct rb_node const *t
);
size_t rbsize(struct rb_node const *node);
size_t rbdepth(struct rb_node const *node);
struct rb_node *rbfromlist(struct rb_assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b));
struct rb_assoc *rbtolist(struct rb_node const *node);
void **rbkeys(struct rb_node const *node);
void **rbvals(struct rb_node const *node);
void rbinvariants(struct rb_node const *node, bool root, int (*keycmp) (void const *a, void const *b));
void print_rbtree(FILE *handle, void (*print_key) (FILE *handle, void const *key, void const *val), struct rb_node const *node);
void free_rbtree(struct rb_node *node);

#endif // BASE_rbtree_H_
