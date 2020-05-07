#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "base/rbtree.h"
#include "base/array.h"
#include "base/ord.h"
#include "base/string.h"

static struct rb_node *rotate_left(struct rb_node *node) {
    if (!node) return NULL;
    if (!node->right) return node;
    struct rb_node *root = node->right;
    node->right = root->left;
    root->left = node;
    return root;
}

static struct rb_node *rotate_right(struct rb_node *node) {
    if (!node) return NULL;
    if (!node->left) return node;
    struct rb_node *root = node->left;
    node->left = root->right;
    root->right = node;
    return root;
}


static struct rb_node
rbtree(void *key, void *val, bool red, struct rb_node *left, struct rb_node *right) {
    return (struct rb_node) { rb_assoc(key, val), red, left, right };
}

struct rb_assoc rb_assoc(void *key, void *val) {
    return (struct rb_assoc) { key, val };
}

struct rb_assoc rb_node_assoc(struct rb_node const *node) {
    return node->assoc;
}

static struct rb_node *
init_rbtree(void *key, void *val, bool red, struct rb_node *left, struct rb_node *right) {
    struct rb_node *node = malloc(sizeof *node);
    assert(node != NULL);
    *node = rbtree(key, val, red, left, right);
    return node;
}

void *rbkey(struct rb_node const *node) { return node ? node->assoc.key : NULL; }
void *rbval(struct rb_node const *node) { return node ? node->assoc.val : NULL; }

static void reset_rb_iter(struct rb_iter *it) {
    struct array *stack = it->stack;
    it->last = NULL;

    if (it->order == RB_PRE) {
        apush(&it->root, stack);
    } else {
        struct rb_node *node = it->root;

        while (node) {
            apush(&node, stack);
            node = node->left;
        }
    }
}

void rb_iter(enum rb_trav_order order, struct rb_node const *node, struct rb_iter *it) {
    struct array *stack = init_array(sizeof node, 7, 0, 0);
    *it = (struct rb_iter) { .stack = stack, .root = (struct rb_node *) node, .order = order };
    reset_rb_iter(it);
}

void free_rb_iter(struct rb_iter *it) {
    if (!it) return;
    free_array(it->stack);
    it->root = it->last = NULL;
    it->stack = NULL;
}

struct rb_node *rbnext(struct rb_iter *it) {
    struct array *stack = it->stack;
    enum rb_trav_order order = it->order;

    // we're resetting
    if (aempty(stack)) {
        reset_rb_iter(it);
        return NULL;
    }

    struct rb_node *node = NULL;
    apop(&node, stack);

    if (order == RB_PRE) {
        if (node->right) apush(&node->right, stack);
        if (node->left)  apush(&node->left, stack);
    } else if (order == RB_POST) {
        struct rb_node *last = it->last;

        if (node->right && node->right != last && (!node->left || node->left == last)) {
            apush(&node, stack);
            node = node->right;
            while (!node->left && node->right) apush(&node, stack), node = node->right;
            while (node->left) apush(&node, stack), node = node->left;
        }

        it->last = node;
    } else if (node->right) {
        struct rb_node *x = node->right;
        while (x) apush(&x, stack), x = x->left;
    }

    return node;
}

struct rb_node *rbmin(struct rb_node *node) {
    if (!node) return NULL;

    if (node->left)
        return rbmin(node->left);

    return node;
}

struct rb_node *rbmax(struct rb_node *node) {
    if (!node) return NULL;

    if (node->right)
        return rbmax(node->right);

    return node;
}

struct rb_node *rbfind(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct rb_node const *node
) {
    assert(keycmp != NULL);

    while (node) {
        int ord = (*keycmp)(key, node->assoc.key);
        if (ord < 0)      node = node->left;
        else if (ord > 0) node = node->right;
        else              break;
    }

    return (struct rb_node *) node;
}

bool rbcontains(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    struct rb_node const *node
) {
    return rbfind(key, keycmp, node) != NULL;
}

static bool red(struct rb_node *node) {
    return node && node->red;
}

static void repaint(struct rb_node *node, bool color) {
    node->red = color;
    if (node->left) node->left->red = !color;
    if (node->right) node->right->red = !color;
}

static bool has_red_child(struct rb_node *node) {
    return red(node->left) || red(node->right);
}

static struct rb_node *balance_after_insert(struct rb_node *node) {
    struct rb_node *left = node->left,
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

static struct rb_node *_rbinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct rb_node *node
) {
    assert(keycmp != NULL);
    if (!node) return init_rbtree(key, val, true, NULL, NULL);

    int ord = (*keycmp)(key, node->assoc.key);
    if      (ord < 0) node->left = _rbinsert(key, keycmp, val, node->left);
    else if (ord > 0) node->right = _rbinsert(key, keycmp, val, node->right);
    else              node->assoc.val = val;

    node = balance_after_insert(node);

    return node;
}

struct rb_node *rbinsert(
    void *key,
    int (*keycmp) (void const *a, void const *b),
    void *val,
    struct rb_node *node
) {
    struct rb_node *root = _rbinsert(key, keycmp, val, node);
    root->red = false;
    return root;
}

bool rbtree_eq(
    bool (*keyeq) (void const *a, void const *b),
    bool (*valeq) (void const *a, void const *b),
    struct rb_node const *s,
    struct rb_node const *t
) {
    bool eq = true;
    assert(keyeq || valeq);
    struct rb_iter si, ti;

    rb_iter(RB_IN, s, &si);
    rb_iter(RB_IN, t, &ti);

    struct rb_node *sn = NULL, *tn = NULL;

    do sn = rbnext(&si), tn = rbnext(&ti);
    while (sn &&
           tn &&
           (eq = keyeq ? (*keyeq)(rbkey(sn), rbkey(tn)) : true &&
                 valeq ? (*valeq)(rbval(sn), rbval(tn)) : true)
          );

    // check that we've exhausted both trees
    eq = sn == NULL && tn == NULL;

    free_rb_iter(&si);
    free_rb_iter(&ti);

    return eq;
}

size_t rbsize(struct rb_node const *node) {
    if (!node) return 0;

    size_t size = 0;

    struct rb_iter it;
    rb_iter(RB_IN, node, &it);

    while ((node = rbnext(&it)))
        size++;

    free_rb_iter(&it);

    return size;
}

size_t rbdepth(struct rb_node const *node) {
    if (!node) return 0;
    return max(rbdepth(node->left), rbdepth(node->right)) + 1;
}

struct rb_node *rbfromlist(struct rb_assoc *assocs, size_t n, int (*keycmp) (void const *a, void const *b)) {
    struct rb_node *node = NULL;

    for (size_t i = 0; i < n; i++) {
        struct rb_assoc a = assocs[i];
        node = rbinsert(a.key, keycmp, a.val, node);
    }

    return node;
}

struct rb_assoc *rbtolist(struct rb_node const *node) {
    if (!node) return NULL;

    struct rb_assoc *assocs = calloc(rbsize(node), sizeof *assocs);

    struct rb_iter it;
    rb_iter(RB_IN, node, &it);

    struct rb_assoc *as = assocs;
    while ((node = rbnext(&it))) {
        *as++ = rb_node_assoc(node);
    }

    free_rb_iter(&it);

    return assocs;
}

void **rbkeys(struct rb_node const *node) {
    if (!node) return NULL;

    void **keys = calloc(rbsize(node), sizeof *keys);

    struct rb_iter it;
    rb_iter(RB_IN, node, &it);

    void **ks = keys;
    while ((node = rbnext(&it))) {
        *ks++ = rbkey(node);
    }

    free_rb_iter(&it);

    return keys;
}

void **rbvals(struct rb_node const *node) {
    if (!node) return NULL;

    void **vals = calloc(rbsize(node), sizeof *vals);

    struct rb_iter it;
    rb_iter(RB_IN, node, &it);

    void **vs = vals;
    while ((node = rbnext(&it))) {
        *vs++ = rbval(node);
    }

    free_rb_iter(&it);

    return vals;
}

static unsigned int black_height(struct rb_node const *node) {
    // leaves are implicitly black
    if (!node) return 1;
    // in theory these should be the same, but because I'm using this routine
    // to check invariants below, it needs to protect against the possibility
    // that I screwed something up
    return max(black_height(node->left), black_height(node->right)) + (node->red ? 0 : 1);
}

void rbinvariants(struct rb_node const *node, bool root, int (*keycmp) (void const *a, void const *b)) {
    assert(keycmp != NULL);
    if (!node) return;

    // post-order traversal
    struct rb_node *left = node->left,
               *right = node->right;

    rbinvariants(left, false, keycmp);
    rbinvariants(right, false, keycmp);

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

    size_t size = rbsize(node), height = rbdepth(node);
    assert(height <= 2 * log2(size + 1));
}

static void _print_rbtree(FILE *handle, void (*print_key) (FILE *handle, void const *key), unsigned int depth, struct rb_node const *node) {
    if (!node || !print_key) return;
    indent(handle, depth);
    fprintf(handle, "("); (*print_key)(handle, node->assoc.key); fprintf(handle, ", %s)", node->red ? "R" : "B"); fprintf(handle, "\n");
    _print_rbtree(handle, print_key, depth + 1, node->left);
    _print_rbtree(handle, print_key, depth + 1, node->right);
}

void print_rbtree(FILE *handle, void (*print_key) (FILE *handle, void const *key), struct rb_node const *node) {
    _print_rbtree(handle, print_key, 0, node);
}

void free_rbtree(struct rb_node *node) {
    if (!node) return;
    free_rbtree(node->left);
    free_rbtree(node->right);
    node->left = node->right = NULL;
    free(node);
}

