#ifndef BASE_BTREE_H_
#define BASE_BTREE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/array.h"

enum traversal_order {
    PRE,
    IN,
    POST
};

struct assoc {
    void *key;
    void *val;
};

struct btnode {
    struct assoc assoc;
    struct btnode *left;
    struct btnode *right;
};

struct btree_iter {
    struct array *stack;
    struct btnode *root;
    struct btnode *last;
    enum traversal_order order;
};

struct btnode btree(void *key, void *val, struct btnode *left, struct btnode *right);
struct assoc assoc(void *key, void *val);
struct assoc assoc_from_node(struct btnode const *node);
struct btnode *init_btree(void *key, void *val, struct btnode *left, struct btnode *right);
void *nodekey(struct btnode const *node);
void *nodeval(struct btnode const *node);
void btree_iter(enum traversal_order order, struct btnode const *node, struct btree_iter *it);
void free_btree_iter(struct btree_iter *it);
struct btnode *btnext(struct btree_iter *it);
struct btnode *btmin(struct btnode const *node);
struct btnode *btmax(struct btnode const *node);
struct btnode *btfind(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct btnode const *node
);
bool btcontains(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct btnode const *node
);
struct btnode *btinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct btnode *node
);
struct btnode *btdelete(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct btnode *node
);
bool btree_eq(
    bool (*keyeq) (void const *a, void const *b),
    bool (*valeq) (void const *a, void const *b),
    struct btnode const *s,
    struct btnode const *t
);
size_t btsize(struct btnode const *node);
size_t btdepth(struct btnode const *node);
struct btnode *btfromlist(struct assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b));
struct assoc *bttolist(struct btnode const *node);
void **btkeys(struct btnode const *node);
void **btvals(struct btnode const *node);
void print_btree(void (*print_key) (void const *key), struct btnode const *node);
void free_btree(struct btnode *node);

#endif // BASE_BTREE_H_
