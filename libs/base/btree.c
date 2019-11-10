#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "base/btree.h"
#include "base/ord.h"

enum ord {
    LT = -1,
    EQ = 0,
    GT = 1
};

static int compare(void const *a, void const *b) {
    int ord = EQ;

    if (*a < *b) {
        ord = LT;
    } else if (*b < *a) {
        ord = GT;
    }

    return ord;
}

struct node btree(void *key, void *val, struct node *left, struct node *right) {
    return (struct node) { assoc(key, val), left, right };
}

struct assoc assoc(void *key, void *val) {
    return (struct assoc) { key, val };
}

struct assoc assoc_from_node(struct node const *node) {
    return (struct assoc) { node->key, node->val };
}

struct node *init_btree(void *key, void *val, struct node *left, struct node *right) {
    struct node *node = malloc(sizeof *node);
    assert(node != NULL);
    *node = btree(key, val, left, right);
    return node;
}

void *nodekey(struct node const *node) { return node ? node->key : NULL; }
void *nodeval(struct node const *node) { return node ? node->val : NULL; }

void btree_iter(enum traversal_order order, struct node const *node, struct btree_iter *it) {
    struct node **stack = calloc(btsize(node), sizeof *stack);
    assert(stack != NULL);
    *it = (struct btree_iter) { .stack = stack, .root = node, .order = order };
}

void free_btree_iter(struct btree_iter *it) {
    if (!it) return;
    free(it->stack);
    it->stack = NULL;
}

struct node *btnext(struct btree_iter *it) {
}

struct node *btprev(struct btree_iter *it) {
}

void *btnextval(struct btree_iter *it) {
}

void *btnextkey(struct btree_iter *it) {
}

void *btprevval(struct btree_iter *it) {
}

void *btprevkey(struct btree_iter *it) {
}

struct node *btmin(struct node const *node) {
    if (!node) return NULL;
    while (node->left) node = node->left;
    return node;
}

struct node *btmax(struct node const *node) {
    if (!node) return NULL;
    while (node->right) node = node->right;
    return node;
}

struct node *btfind(void *key, int (*keycmp) (void const *a, void const *b), struct node const *node) {
    if (!keycmp) keycmp = compare;

    while (node) {
        int ord = (*keycmp)(key, node->key);
        if (ord < 0)      node = node->left;
        else if (ord > 0) node = node->right;
        else              break;
    }

    return node;
}

struct node *btinsert(void *key, int (*keycmp) (void const *a, void const *b), void *val, struct node *node) {
    if (!node) return init_btree(key, val, NULL, NULL);
    if (!keycmp) keycmp = compare;

    struct node *root = node, *last = NULL;
    int ord;

    while (node) {
        last = node;

        ord = (*keycmp)(key, node->key);
        if (ord < 0)      node = node->left;
        else if (ord > 0) node = node->right;
        else              break;
    }

    if (ord < 0)      last->left = init_btree(key, val, NULL, NULL);
    else if (ord > 0) last->right = init_btree(key, val, NULL, NULL);
    else              last->val = val;

    return root;
}

struct node *btdelete(void *key, int (*keycmp) (void const *a, void const *b), struct node *node) {
    if (!node) return NULL;
    if (!keycmp) keycmp = compare;

    struct node *root = node, *parent = NULL;
    int ord;

    while (node) {
        ord = (*keycmp)(key, node->key);
        if (ord < 0)      parent = node; node = node->left;
        else if (ord > 0) parent = node; node = node->right;
        else              break;
    }

    if (!node) return root;

    // compute the next subtree given node
    struct node *next = NULL;
    if (!node->left)        next = node->right;
    else if (!node->right)  next = node->left;
    else {
        struct node *nextp = NULL;

        next = node->right;
        while (next->left) {
            nextp = next;
            next = next->left;
        }

        if (nextp) {
            nextp->left = next->right;
            next->right = node->right;
        }

        next->left = node->left;
    }

    // update parent pointers or replace parent with the next subtree
    if (!parent)                   root = next;
    else if (parent->left == node) parent->left = next;
    else                           parent->right = next;

    free_btree(node);

    return root;
}

size_t btsize(struct node const *node) {
    if (!node) return 0;

    size_t size = 0;

    struct btree_iter it;
    btree_iter(&it, IN, node);

    while ((node = btnext(&it)))
        size++;

    free_btree_iter(&it);

    return size;
}

size_t btdepth(struct node const *node) {
    if (!node) return 0;
    return max(btdepth(node->left), btdepth(node->right)) + 1;
}

struct node *btfromlist(struct assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b)) {
    struct node *node = NULL;

    for (int i = 0; i < n; i++) {
        struct assoc a = assocs[i];
        node = btinsert(a.key, keycmp, a.val, node);
    }

    return node;
}

struct assoc *bttolist(struct node const *node) {
    if (!node) return NULL;

    struct assoc *assocs = calloc(btsize(node), sizeof *assocs);

    struct btree_iter it;
    btree_iter(&it, IN, node);

    struct assoc *as = assocs;
    while ((node = btnext(node))) {
        *as++ = assoc_from_node(node);
    }

    free_btree_iter(&it);

    return assocs;
}

void **btkeys(struct node const *node) {
    if (!node) return NULL;

    void **keys = calloc(btsize(node), sizeof *keys);

    struct btree_iter it;
    btree_iter(&it, IN, node);

    void *ks = keys;
    while ((node = btnext(node))) {
        *ks++ = nodekey(node);
    }

    free_btree_iter(&it);

    return assocs;
}

void **btvals(struct node const *node) {
    if (!node) return NULL;

    void **vals = calloc(btsize(node), sizeof *vals);

    struct btree_iter it;
    btree_iter(&it, IN, node);

    void *vs = vals;
    while ((node = btnext(node))) {
        *vs++ = nodeval(node);
    }

    free_btree_iter(&it);

    return vals;
}

void free_btree(struct node *node) {
    if (!node) return;
    free_btree(node->left);
    free_btree(node->right);
    node->left = node->right = NULL;
    free(node);
}

