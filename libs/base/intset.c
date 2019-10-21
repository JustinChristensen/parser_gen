#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include "base/intset.h"
#include "base/ord.h"
#include "base/string.h"
#include "base/array.h"
#include "base/bits.h"

static bool is_branch(struct intset const *set) {
    return set->left != NULL;
}

static bool prefix_upto_branch_matches(int64_t kfix, struct intset const *set) {
    return prefix_upto_branch(kfix, set->mask) == set->pfix;
}

static bool bmchecki(int64_t *out, int64_t pfix, int64_t bitmap, int i) {
    // check the ith bit
    int64_t x = bitmap & (BIT << i);

    // we found a set bit, notify the caller
    if (x) {
        *out = pfix | ctz(x);
        return true;
    }

    return false;
}

struct intset intset(int64_t pfix, int64_t mask, struct intset *left, struct intset *right) {
    return (struct intset) { pfix, mask, left, right };
}

struct intset *init_intset(int64_t pfix, int64_t mask, struct intset *left, struct intset *right) {
    struct intset *set = malloc(sizeof *set);
    assert(set != NULL);
    *set = intset(pfix, mask, left, right);
    return set;
}

bool siterator(struct intset const *set, struct intset_iterator *it) {
    if (!it || !set) return false;
    it->stack = init_array(sizeof set, IT_STACK_SIZE, 0, 0);
    reset_siterator(it);
    it->root = set;
    return true;
}

// iterate all nodes
bool snextnode(struct intset const **out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct array *stack = it->stack;

    // if we're at the root, push the root node onto the stack
    if (it->at_root) apush(&it->root, it->stack);

    if (aempty(stack)) {
        *out = NULL;
        reset_siterator(it);
        return false;
    } else {
        struct intset *set = NULL;

        apop(&set, stack);

        if (is_branch(set)) {
            // if we're not at the root or the prefix is not negative
            // go right first (larger numbers)
            if (it->at_root && set->mask < 0) {
                apush(&set->left, stack);
                apush(&set->right, stack);
            } else {
                apush(&set->right, stack);
                apush(&set->left, stack);
            }
        }

        // used by the bitmap and int iterators
        // DO NOT REMOVE
        it->set = set;
        it->at_root = false;

        // output
        *out = set;

        return true;
    }
}

// iterate leaf nodes
bool snextleaf(struct intset const **out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct intset const *set = NULL;
    bool res = true;

    while ((res = snextnode(&set, it))) {
        // skip branches
        if (is_branch(set)) continue;
        break;
    }

    // output
    *out = set;

    return res;
}

bool snextbitmap(int *out, struct intset_iterator *it) {
    if (!out || !it || !it->set) return false;

    struct intset const *set = it->set;

    int64_t x;
    for (int i = it->i; i < WORDBITS; (it->i = ++i)) {
        if (bmchecki(&x, set->pfix, set->mask, i)) {
            it->i++;
            *out = (int) x;
            return true;
        }
    }

    it->i = 0;

    return false;
}

bool snext(int *out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct intset const *set = it->set;

    while (true) {
        if (it->i == 0 && !snextleaf(&set, it))
            break;

        if (snextbitmap(out, it))
            return true;
    }

    return false;
}

void reset_siterator(struct intset_iterator *it) {
    it->at_root = true;
    it->set = NULL;
    it->i = 0;
    areset(it->stack);
}

bool selem(int k, struct intset const *set) {
    struct array *stack = init_array(sizeof set, IT_STACK_SIZE, 0, 0);
    bool elem = false;
    int64_t kfix = prefix(k),
            bmap = bitmap(k);

    apush(&set, stack);

    while (!aempty(stack)) {
        apop(&set, stack);

        if (is_branch(set)) {
            if (prefix_upto_branch_matches(kfix, set)) {
                if (zero(kfix, set->mask)) {
                    apush((void **) &set->left, stack);
                } else {
                    apush((void **) &set->right, stack);
                }

                continue;
            }
        } else if (set->pfix == kfix && (set->mask & bmap) != 0) {
            elem = true;
        }

        break;
    }

    free_array(stack);

    return elem;
}

static struct intset *link(int64_t kfix, struct intset *newleaf, struct intset *set) {
    struct intset *right = newleaf;
    int64_t brm = branch_mask(kfix, set->pfix);

    if (zero(kfix, brm)) {
        right = set;
        set = newleaf;
    }

    // the new branch node will have the leading bits of the key
    // up to the where the key and the keys in the branch diverge
    return init_intset(prefix_upto_branch(kfix, brm), brm, set, right);
}

static struct intset *_sinsert(int64_t kfix, int64_t bitmap, struct intset *set) {
    if (!set) { // nil
        set = init_intset(kfix, bitmap, NULL, NULL);
    } else if (set->left) { // branch node
        // do the key's prefix and branch prefix match?
        if (!prefix_upto_branch_matches(kfix, set)) {
            set = link(kfix, init_intset(kfix, bitmap, NULL, NULL), set);
        } else if (zero(kfix, set->mask)) { // branch does not match, go left
            set->left = _sinsert(kfix, bitmap, set->left);
        } else { // branch matches, go right
            set->right = _sinsert(kfix, bitmap, set->right);
        }
    } else  { // leaf node
        if (set->pfix == kfix) {
            set->mask |= bitmap;
        } else {
            set = link(kfix, init_intset(kfix, bitmap, NULL, NULL), set);
        }
    }

    return set;
}

struct intset *sinsert(int k, struct intset *set) {
    return _sinsert(prefix(k), bitmap(k), set);
}

struct intset *slistinsert(int *k, size_t n, struct intset *set) {
    for (int i = 0; i < n; i++) {
        set = sinsert(k[i], set);
    }

    return set;
}

// void sdelete(int k, struct intset *set) {
// }
//
// bool intseteq(struct intset const *a, struct intset const *b) {
// }
//
// struct intset *sunion(struct intset *a, struct intset const *b) {
// }
//
// struct intset *sintersection(struct intset *a, struct intset const *b) {
// }
//
// bool sdisjoint(struct intset *a, struct intset const *b) {
// }
//

bool snull(struct intset const *set) {
    return ssize(set) == 0;
}

size_t ssize(struct intset const *set) {
    if (!set) return 0;

    size_t n = 0;

    struct intset_iterator it;
    if (siterator(set, &it)) {
        int x;
        while (snext(&x, &it)) n++;
        free_siterator(&it);
    }

    return n;
}

size_t streesize(struct intset const *set) {
    if (!set) return 0;

    size_t n = 0;

    struct intset_iterator it;
    if (siterator(set, &it)) {
        while (snextnode(&set, &it)) n++;
        free_siterator(&it);
    }

    return n;
}

static size_t _streedepth(struct intset const *set, int d) {
    if (!set) return d;
    d++;
    return max(_streedepth(set->left, d), _streedepth(set->right, d));
}

size_t streedepth(struct intset const *set) {
    return _streedepth(set, 0);
}

void print_intset(struct intset const *set) {
    if (!set) return;

    struct intset_iterator it;
    if (siterator(set, &it)) {
        int x;
        while (snext(&x, &it)) {
            printf("%d ", x);
        }

        free_siterator(&it);
    }
}

static void _print_intset_tree(struct intset const *set, int depth) {
    if (!set) return;
    indent(depth);
    printf("%s %p (%"PRId64", ", set->left ? "branch" : "leaf", set, set->pfix);
    printbits(set->mask);
    printf(")\n");
    depth++;
    _print_intset_tree(set->left, depth);
    _print_intset_tree(set->right, depth);
}

void print_intset_tree(struct intset const *set) {
    _print_intset_tree(set, 0);
}

void free_intset(struct intset *set) {
    if (!set) return;
    free_intset(set->left);
    free_intset(set->right);
    free(set);
}

void free_siterator(struct intset_iterator *it) {
    if (!it) return;
    free_array(it->stack);
    it->stack = NULL;
}
