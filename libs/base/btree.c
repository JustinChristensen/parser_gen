#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "base/btree.h"
#include "base/array.h"
#include "base/ord.h"
#include "base/string.h"

struct bin btree(void *key, void *val, struct bin *left, struct bin *right) {
    return (struct bin) { assoc(key, val), left, right };
}

struct assoc assoc(void *key, void *val) {
    return (struct assoc) { key, val };
}

struct assoc node_assoc(struct bin const *node) {
    return node->assoc;
}

struct bin *init_btree(void *key, void *val, struct bin *left, struct bin *right) {
    struct bin *node = malloc(sizeof *node);
    assert(node != NULL);
    *node = btree(key, val, left, right);
    return node;
}

void *nodekey(struct bin const *node) { return node ? node->assoc.key : NULL; }
void *nodeval(struct bin const *node) { return node ? node->assoc.val : NULL; }

static void reset_btree_iter(struct btree_iter *it) {
    struct array *stack = it->stack;
    it->last = NULL;

    if (it->order == PRE) {
        apush(&it->root, stack);
    } else {
        struct bin *node = it->root;

        while (node) {
            apush(&node, stack);
            node = node->left;
        }
    }
}

void btree_iter(enum traversal_order order, struct bin const *node, struct btree_iter *it) {
    struct array *stack = init_array(sizeof node, 7, 0, 0);
    *it = (struct btree_iter) { .stack = stack, .root = (struct bin *) node, .order = order };
    reset_btree_iter(it);
}

void free_btree_iter(struct btree_iter *it) {
    if (!it) return;
    free_array(it->stack);
    it->root = it->last = NULL;
    it->stack = NULL;
}

struct bin *btnext(struct btree_iter *it) {
    struct array *stack = it->stack;
    enum traversal_order order = it->order;

    // we're resetting
    if (aempty(stack)) {
        reset_btree_iter(it);
        return NULL;
    }

    struct bin *node = NULL;
    apop(&node, stack);

    if (order == PRE) {
        if (node->right) apush(&node->right, stack);
        if (node->left)  apush(&node->left, stack);
    } else if (order == POST) {
        struct bin *last = it->last;

        if (node->right && node->right != last && (!node->left || node->left == last)) {
            apush(&node, stack);
            node = node->right;
            while (!node->left && node->right) apush(&node, stack), node = node->right;
            while (node->left) apush(&node, stack), node = node->left;
        }

        it->last = node;
    } else if (node->right) {
        struct bin *x = node->right;
        while (x) apush(&x, stack), x = x->left;
    }

    return node;
}

struct bin *btmin(struct bin const *node) {
    if (!node) return NULL;
    while (node->left) node = node->left;
    return (struct bin *) node;
}

struct bin *btmax(struct bin const *node) {
    if (!node) return NULL;
    while (node->right) node = node->right;
    return (struct bin *) node;
}

struct bin *btfind(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin const *node
) {
    assert(keycmp != NULL);

    while (node) {
        int ord = (*keycmp)(key, node->assoc.key);
        if (ord < 0)      node = node->left;
        else if (ord > 0) node = node->right;
        else              break;
    }

    return (struct bin *) node;
}

bool btcontains(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin const *node
) {
    return btfind(key, keycmp, node) != NULL;
}

struct bin *btinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct bin *node
) {
    assert(keycmp != NULL);
    if (!node) return init_btree(key, val, NULL, NULL);

    struct bin *root = node, *last = NULL;
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

struct bin *btdelete(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin *node
) {
    assert(keycmp != NULL);
    if (!node) return NULL;

    struct bin *root = node, *parent = NULL;
    int ord;

    while (node) {
        ord = (*keycmp)(key, node->assoc.key);
        if (ord < 0)      parent = node, node = node->left;
        else if (ord > 0) parent = node, node = node->right;
        else              break;
    }

    if (!node) return root;

    // compute the next subtree given node
    struct bin *next = NULL;
    if (!node->left)        next = node->right;
    else if (!node->right)  next = node->left;
    else {
        struct bin *nextp = NULL;

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

    free(node);

    return root;
}

bool btree_eq(
    bool (*keyeq) (void const *a, void const *b),
    bool (*valeq) (void const *a, void const *b),
    struct bin const *s,
    struct bin const *t
) {
    bool eq = true;
    assert(keyeq || valeq);
    struct btree_iter si, ti;

    btree_iter(IN, s, &si);
    btree_iter(IN, t, &ti);

    struct bin *sn = NULL, *tn = NULL;

    do sn = btnext(&si), tn = btnext(&ti);
    while (sn &&
           tn &&
           (eq = keyeq ? (*keyeq)(nodekey(sn), nodekey(tn)) : true &&
                 valeq ? (*valeq)(nodeval(sn), nodeval(tn)) : true)
          );

    // check that we've exhausted both trees
    eq = sn == NULL && tn == NULL;

    free_btree_iter(&si);
    free_btree_iter(&ti);

    return eq;
}

size_t btsize(struct bin const *node) {
    if (!node) return 0;

    size_t size = 0;

    struct btree_iter it;
    btree_iter(IN, node, &it);

    while ((node = btnext(&it)))
        size++;

    free_btree_iter(&it);

    return size;
}

size_t btdepth(struct bin const *node) {
    if (!node) return 0;
    return max(btdepth(node->left), btdepth(node->right)) + 1;
}

struct bin *btfromlist(struct assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b)) {
    struct bin *node = NULL;

    for (int i = 0; i < n; i++) {
        struct assoc a = assocs[i];
        node = btinsert(a.key, keycmp, a.val, node);
    }

    return node;
}

struct assoc *bttolist(struct bin const *node) {
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

void **btkeys(struct bin const *node) {
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

void **btvals(struct bin const *node) {
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

static void _print_btree(void (*print_key) (void const *key), unsigned int depth, struct bin const *node) {
    if (!node || !print_key) return;
    indent(depth); printf("("); (*print_key)(node->assoc.key); printf(")"); printf("\n");
    _print_btree(print_key, depth + 1, node->left);
    _print_btree(print_key, depth + 1, node->right);
}

void print_btree(void (*print_key) (void const *key), struct bin const *node) {
    _print_btree(print_key, 0, node);
}

void free_btree(struct bin *node) {
    if (!node) return;
    free_btree(node->left);
    free_btree(node->right);
    node->left = node->right = NULL;
    free(node);
}

