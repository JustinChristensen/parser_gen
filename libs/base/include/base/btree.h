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

struct bin {
    struct assoc assoc;
    bool red;
    struct bin *left;
    struct bin *right;
};

struct btree_iter {
    struct array *stack;
    struct bin *root;
    struct bin *last;
    enum traversal_order order;
};

struct bin btree(void *key, void *val, bool red, struct bin *left, struct bin *right);
struct assoc assoc(void *key, void *val);
struct assoc assoc_from_node(struct bin const *node);
struct bin *init_btree(void *key, void *val, bool red, struct bin *left, struct bin *right);
void *nodekey(struct bin const *node);
void *nodeval(struct bin const *node);
void btree_iter(enum traversal_order order, struct bin const *node, struct btree_iter *it);
void free_btree_iter(struct btree_iter *it);
struct bin *btnext(struct btree_iter *it);
struct bin *btmin(struct bin *node);
struct bin *btmax(struct bin *node);
struct bin *btfind(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin const *node
);
bool btcontains(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin const *node
);
struct bin *btinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct bin *node
);
struct bin *btdelete(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin *node
);
bool btree_eq(
    bool (*keyeq) (void const *a, void const *b),
    bool (*valeq) (void const *a, void const *b),
    struct bin const *s,
    struct bin const *t
);
size_t btsize(struct bin const *node);
size_t btdepth(struct bin const *node);
bool btbalanced(struct bin const *node);
struct bin *btfromlist(struct assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b));
struct assoc *bttolist(struct bin const *node);
void **btkeys(struct bin const *node);
void **btvals(struct bin const *node);
void btinvariants(struct bin const *node, bool root, int (*keycmp) (void const *a, void const *b));
void print_btree(void (*print_key) (void const *key), struct bin const *node);
void free_btree(struct bin *node);

#endif // BASE_BTREE_H_
