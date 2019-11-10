#ifndef BASE_BTREE_H_
#define BASE_BTREE_H_ 1

#include <stdlib.h>

enum traversal_order {
    PRE,
    IN,
    POST
};

struct assoc {
    void *key;
    void *val;
};

struct node {
    struct assoc assoc;
    struct node *left;
    struct node *right;
};

struct btree_iter {
    struct node **stack;
    struct node *root;
    enum traversal_order order;
};

struct node btree(void *key, void *val, struct node *left, struct node *right);
struct assoc assoc(void *key, void *val);
struct assoc assoc_from_node(struct node const *node);
struct node *init_btree(void *key, void *val, struct node *left, struct node *right);
void *nodekey(struct node const *node);
void *nodeval(struct node const *node);
void btree_iter(struct btree_iter *it, enum traversal_order order, struct node const *node);
void free_btree_iter(struct btree_iter *it);
struct node *btnext(struct btree_iter *it);
struct node *btprev(struct btree_iter *it);
void *btnextval(struct btree_iter *it);
void *btnextkey(struct btree_iter *it);
void *btprevval(struct btree_iter *it);
void *btprevkey(struct btree_iter *it);
struct node *btmin(struct node const *node);
struct node *btmax(struct node const *node);
struct node *btfind(void *key, int (*keycmp) (void const *a, void const *b), struct node const *node);
struct node *btinsert(void *key, void *val, struct node *node, int (*keycmp) (void const *a, void const *b));
void btdelete(void *key, int (*keycmp) (void const *a, void const *b), struct node *node);
size_t btsize(struct node const *node);
size_t btdepth(struct node const *node);
struct node *btfromlist(struct assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b));
struct assoc *bttolist(struct node const *node);
void **btkeys(struct node const *node);
void **btvals(struct node const *node);
void free_btree(struct node *node);

#endif // BASE_BTREE_H_
