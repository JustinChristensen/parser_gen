#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "base/btree.h"
#include "base/array.h"
#include "base/ord.h"
#include "base/string.h"

static struct bin *rotate_left(struct bin *node) {
    if (!node) return NULL;
    if (!node->right) return node;
    struct bin *root = node->right;
    node->right = root->left;
    root->left = node;
    return root;
}

static struct bin *rotate_right(struct bin *node) {
    if (!node) return NULL;
    if (!node->left) return node;
    struct bin *root = node->left;
    node->left = root->right;
    root->right = node;
    return root;
}


struct bin btree(void *key, void *val, bool red, struct bin *left, struct bin *right) {
    return (struct bin) { assoc(key, val), red, left, right };
}

struct assoc assoc(void *key, void *val) {
    return (struct assoc) { key, val };
}

struct assoc node_assoc(struct bin const *node) {
    return node->assoc;
}

struct bin *init_btree(void *key, void *val, bool red, struct bin *left, struct bin *right) {
    struct bin *node = malloc(sizeof *node);
    assert(node != NULL);
    *node = btree(key, val, red, left, right);
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

struct bin *btmin(struct bin *node) {
    if (!node) return NULL;

    if (node->left)
        return btmin(node->left);

    return node;
}

struct bin *btmax(struct bin *node) {
    if (!node) return NULL;

    if (node->right)
        return btmax(node->right);

    return node;
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

static bool red(struct bin *node) {
    return node && node->red;
}

static void repaint(struct bin *node, bool color) {
    node->red = color;
    if (node->left) node->left->red = !color;
    if (node->right) node->right->red = !color;
}

static bool has_red_child(struct bin *node) {
    return red(node->left) || red(node->right);
}

static struct bin *balance_after_insert(struct bin *node) {
    struct bin *left = node->left,
               *right = node->right;

    if (red(left) && red(right) && (has_red_child(left) || has_red_child(right))) {
        repaint(node, true);
    } else if (red(left)) {
        if (red(left->right))
            left = node->left = rotate_left(left);

        if (red(left->left)) {
            node = rotate_right(node);
            repaint(node, false);
        }
    } else if (red(right)) {
        if (red(right->left))
            right = node->right = rotate_right(right);

        if (red(right->right)) {
            node = rotate_left(node);
            repaint(node, false);
        }
    }

    return node;
}

static struct bin *_btinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct bin *node
) {
    assert(keycmp != NULL);
    if (!node) return init_btree(key, val, true, NULL, NULL);

    int ord = (*keycmp)(key, node->assoc.key);
    if      (ord < 0) node->left = _btinsert(key, keycmp, val, node->left);
    else if (ord > 0) node->right = _btinsert(key, keycmp, val, node->right);
    else              node->assoc.val = val;

    node = balance_after_insert(node);

    return node;
}

struct bin *btinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct bin *node
) {
    struct bin *root = _btinsert(key, keycmp, val, node);
    root->red = false;
    return root;
}

static struct bin *balance_after_delete(struct bin *node) {
    if (!node) return node;

    struct bin *left = node->left,
               *right = node->right;

    if (!left && right) {
        if (!right->red && right->left) {
            // case #3:
            // node->right has one or two red children (part of a 3/4-node)
            // node is either red or black
            // rotate right to form a R B (R) chain
            right = node->right = rotate_right(right);
        }

        if (!right->right) {
            // case #2:
            // right has no red nodes to steal
            // TODO: how to handle this?
        } else {
            // case #1/4:
            // node->right is red with at least one black child
            // node is red or black (and part of a 3-node)
            // paint and rotate to pull node from parent 3-node

            if (right->left) // right is red, right->left is black
                right->left->red = true;

            right->red = node->red;
            node->red = false;
            node = rotate_left(node);
        }
    } else if (!right && left) {
        if (!left->red && left->right)
            left = node->left = rotate_left(left);

        if (!left->left) {
            // TODO: how to handle this?
        } else {
            if (left->right)
                left->right->red = true;

            left->red = node->red;
            node->red = false;
            node = rotate_right(node);
        }
    }

    return node;
}

static struct bin *grab_min(struct bin **min, struct bin *node) {
    if (!node) return NULL;

    if (node->left) {
        node->left = grab_min(min, node->left);
        return node;
    }

    *min = node;
    return node->right;
}

static struct bin *_btdelete(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin *node
) {
    assert(keycmp != NULL);
    if (!node) return NULL;

    int ord = (*keycmp)(key, node->assoc.key);
    if      (ord < 0) node->left = _btdelete(key, keycmp, node->left);
    else if (ord > 0) node->right = _btdelete(key, keycmp, node->right);
    else {
        struct bin *next = NULL;

        if (!node->left)       next = node->right;
        else if (!node->right) next = node->left;
        else {
            next->right = grab_min(&next, node->right);
            next->left = node->left;
        }

        if (next) next->red = node->red;

        free(node);
        node = next;
    }

    node = balance_after_delete(node);

    return node;
}

struct bin *btdelete(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct bin *node
) {
    struct bin *root = _btdelete(key, keycmp, node);
    root->red = false;
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

bool btbalanced(struct bin const *node) {
    if (!node) return true;
    return btdepth(node->left) == btdepth(node->right);
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

static unsigned int black_height(struct bin const *node) {
    // leaves are implicitly black
    if (!node) return 1;
    // in theory these should be the same, but because I'm using this routine
    // to check invariants below, it needs to protect against the possibility
    // that I screwed something up
    return max(black_height(node->left), black_height(node->right)) + (node->red ? 0 : 1);
}

void btinvariants(struct bin const *node, bool root, int (*keycmp) (void const *a, void const *b)) {
    assert(keycmp != NULL);
    if (!node) return;

    // post-order traversal
    struct bin *left = node->left,
               *right = node->right;

    btinvariants(left, false, keycmp);
    btinvariants(right, false, keycmp);

    // for all nodes n, keys in node->left are less than n's key
    // and keys in node->right are greater than n's key
    if (left) assert((*keycmp)(left->assoc.key, node->assoc.key) < 0);
    if (right) assert((*keycmp)(right->assoc.key, node->assoc.key) > 0);

    // the root is black
    if (root) assert(!node->red);

    // if a node is red, then both it's children are black
    if (node->red)
        assert(left == NULL && right == NULL || !left->red && !right->red);

    // all simple paths from the node to it's descendant leaves contain the
    // same number of black nodes
    assert(black_height(left) == black_height(right));

    size_t size = btsize(node), height = btdepth(node);
    assert(height <= 2 * log2(size + 1));
}

static void _print_btree(void (*print_key) (void const *key), unsigned int depth, struct bin const *node) {
    if (!node || !print_key) return;
    indent(depth); printf("("); (*print_key)(node->assoc.key); printf(", %s)", node->red ? "R" : "B"); printf("\n");
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

