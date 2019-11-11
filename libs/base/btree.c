#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "base/btree.h"
#include "base/array.h"
#include "base/ord.h"
#include "base/string.h"

struct btnode btree(void *key, void *val, struct btnode *left, struct btnode *right) {
    return (struct btnode) { assoc(key, val), left, right };
}

struct assoc assoc(void *key, void *val) {
    return (struct assoc) { key, val };
}

struct assoc node_assoc(struct btnode const *node) {
    return node->assoc;
}

struct btnode *init_btree(void *key, void *val, struct btnode *left, struct btnode *right) {
    struct btnode *node = malloc(sizeof *node);
    assert(node != NULL);
    *node = btree(key, val, left, right);
    return node;
}

void *nodekey(struct btnode const *node) { return node ? node->assoc.key : NULL; }
void *nodeval(struct btnode const *node) { return node ? node->assoc.val : NULL; }

static void reset_btree_iter(struct btree_iter *it) {
    struct array *stack = it->stack;
    it->last = NULL;

    if (it->order == PRE) {
        apush(&it->root, stack);
    } else {
        struct btnode *node = it->root;

        while (node) {
            apush(&node, stack);
            node = node->left;
        }
    }
}

void btree_iter(enum traversal_order order, struct btnode const *node, struct btree_iter *it) {
    struct array *stack = init_array(sizeof node, 7, 0, 0);
    *it = (struct btree_iter) { .stack = stack, .root = (struct btnode *) node, .order = order };
    reset_btree_iter(it);
}

void free_btree_iter(struct btree_iter *it) {
    if (!it) return;
    free_array(it->stack);
    it->root = it->last = NULL;
    it->stack = NULL;
}

struct btnode *btnext(struct btree_iter *it) {
    struct array *stack = it->stack;
    enum traversal_order order = it->order;

    // we're resetting
    if (aempty(stack)) {
        reset_btree_iter(it);
        return NULL;
    }

    struct btnode *node = NULL;
    apop(&node, stack);

    if (order == PRE) {
        if (node->right) apush(&node->right, stack);
        if (node->left)  apush(&node->left, stack);
    } else if (order == POST) {
        struct btnode *last = it->last;

        if (node->right && node->right != last && (!node->left || node->left == last)) {
            apush(&node, stack);
            node = node->right;
            while (node) apush(&node, stack), node = node->left;
            apop(&node, stack);
        }

        it->last = node;
    } else if (node->right) {
        struct btnode *x = node->right;
        while (x) apush(&x, stack), x = x->left;
    }

    return node;
}

struct btnode *btmin(struct btnode const *node) {
    if (!node) return NULL;
    while (node->left) node = node->left;
    return (struct btnode *) node;
}

struct btnode *btmax(struct btnode const *node) {
    if (!node) return NULL;
    while (node->right) node = node->right;
    return (struct btnode *) node;
}

struct btnode *btfind(void *key, int (*keycmp) (void const *a, void const *b), struct btnode const *node) {
    assert(keycmp != NULL);

    while (node) {
        int ord = (*keycmp)(key, node->assoc.key);
        if (ord < 0)      node = node->left;
        else if (ord > 0) node = node->right;
        else              break;
    }

    return (struct btnode *) node;
}

struct btnode *btinsert(void *key, int (*keycmp) (void const *a, void const *b), void *val, struct btnode *node) {
    assert(keycmp != NULL);
    if (!node) return init_btree(key, val, NULL, NULL);

    struct btnode *root = node, *last = NULL;
    int ord;

    while (node) {
        last = node;

        ord = (*keycmp)(key, node->assoc.key);
        if (ord < 0)      node = node->left;
        else if (ord > 0) node = node->right;
        else              break;
    }

    if (ord < 0)      last->left = init_btree(key, val, NULL, NULL);
    else if (ord > 0) last->right = init_btree(key, val, NULL, NULL);
    else              last->assoc.val = val;

    return root;
}

struct btnode *btdelete(void *key, int (*keycmp) (void const *a, void const *b), struct btnode *node) {
    assert(keycmp != NULL);
    if (!node) return NULL;

    struct btnode *root = node, *parent = NULL;
    int ord;

    while (node) {
        ord = (*keycmp)(key, node->assoc.key);
        if (ord < 0)      parent = node, node = node->left;
        else if (ord > 0) parent = node, node = node->right;
        else              break;
    }

    if (!node) return root;

    // compute the next subtree given node
    struct btnode *next = NULL;
    if (!node->left)        next = node->right;
    else if (!node->right)  next = node->left;
    else {
        struct btnode *nextp = NULL;

        next = node->right;

        while (next->left) {
            nextp = next, next = next->left;
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

size_t btsize(struct btnode const *node) {
    if (!node) return 0;

    size_t size = 0;

    struct btree_iter it;
    btree_iter(IN, node, &it);

    while ((node = btnext(&it)))
        size++;

    free_btree_iter(&it);

    return size;
}

size_t btdepth(struct btnode const *node) {
    if (!node) return 0;
    return max(btdepth(node->left), btdepth(node->right)) + 1;
}

struct btnode *btfromlist(struct assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b)) {
    struct btnode *node = NULL;

    for (int i = 0; i < n; i++) {
        struct assoc a = assocs[i];
        node = btinsert(a.key, keycmp, a.val, node);
    }

    return node;
}

struct assoc *bttolist(struct btnode const *node) {
    if (!node) return NULL;

    struct assoc *assocs = calloc(btsize(node), sizeof *assocs);

    struct btree_iter it;
    btree_iter(IN, node, &it);

    struct assoc *as = assocs;
    while ((node = btnext(&it))) {
        *as++ = node_assoc(node);
    }

    free_btree_iter(&it);

    return assocs;
}

void **btkeys(struct btnode const *node) {
    if (!node) return NULL;

    void **keys = calloc(btsize(node), sizeof *keys);

    struct btree_iter it;
    btree_iter(IN, node, &it);

    void **ks = keys;
    while ((node = btnext(&it))) {
        *ks++ = nodekey(node);
    }

    free_btree_iter(&it);

    return keys;
}

void **btvals(struct btnode const *node) {
    if (!node) return NULL;

    void **vals = calloc(btsize(node), sizeof *vals);

    struct btree_iter it;
    btree_iter(IN, node, &it);

    void **vs = vals;
    while ((node = btnext(&it))) {
        *vs++ = nodeval(node);
    }

    free_btree_iter(&it);

    return vals;
}

static void _print_btree(void (*print_key) (void const *key), unsigned int depth, struct btnode const *node) {
    if (!node || !print_key) return;
    indent(depth); (*print_key)(node->assoc.key); printf("\n");
    _print_btree(print_key, depth + 1, node->left);
    _print_btree(print_key, depth + 1, node->right);
}

void print_btree(void (*print_key) (void const *key), struct btnode const *node) {
    _print_btree(print_key, 0, node);
}

void free_btree(struct btnode *node) {
    if (!node) return;
    free_btree(node->left);
    free_btree(node->right);
    node->left = node->right = NULL;
    free(node);
}

