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

static bool bmchecki(uint64_t *out, uint64_t pfix, uint64_t bitmap, int i) {
    // check the ith bit
    uint64_t x = bitmap & (BIT << i);

    // we found a set bit, notify the caller
    if (x) {
        *out = pfix | ctzll(x);
        return true;
    }

    return false;
}

struct intset intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    return (struct intset) { pfix, mask, left, right };
}

struct intset *init_intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    struct intset *set = malloc(sizeof *set);
    assert(set != NULL);
    *set = intset(pfix, mask, left, right);
    return set;
}

bool isiterator(struct intset const *set, struct intset_iterator *it) {
    if (!it || !set) return false;
    it->stack = init_array(sizeof set, IT_STACK_SIZE, 0, 0);
    reset_isiterator(it);
    it->i = 0;
    apush(&set, it->stack);
    return true;
}

bool isnext64bm(int64_t *out, struct intset_iterator *it) {
    if (!out || !it || !it->set) return false;

    struct intset *set = it->set;

    // reset the counter
    if (it->i >= WORDBITS)
        it->i = 0;

    uint64_t x;
    for (int i = it->i; i < WORDBITS; (it->i = ++i)) {
        if (bmchecki(&x, set->pfix, set->mask, i)) {
            it->i++;
            *out = x;
            return true;
        }
    }

    return false;
}

bool isnextbm(int *out, struct intset_iterator *it) {
    if (!out || !it) return false;
    int64_t i;
    bool res = isnext64bm(&i, it);
    *out = (int) i;
    return res;
}

// iterate all nodes
bool isnextn(struct intset const **out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct array *stack = it->stack;

    if (aempty(stack)) {
        *out = NULL;
        reset_isiterator(it);
        return false;
    } else {
        struct intset *set = NULL;

        apop(&set, stack);

        if (set->left) {
            // if we're not at the root or the prefix is not negative
            // go right first (larger numbers)
            if (!it->root || set->pfix <= INT64_MAX) {
                apush(&set->left, stack);
                apush(&set->right, stack);
            } else { // go left first to list negatives
                apush(&set->right, stack);
                apush(&set->left, stack);
            }
        }

        // used by the bitmap and int iterators
        // DO NOT REMOVE
        it->set = set;
        it->root = false;

        // output
        *out = set;

        return true;
    }
}

// iterate leaf nodes
bool isnextl(struct intset const **out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct intset const *set = NULL;
    bool res = true;

    while ((res = isnextn(&set, it))) {
        // skip branches
        if (set->left) continue;
        break;
    }

    // output
    *out = set;

    return res;
}

bool isnext64(int64_t *out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct intset const *set = it->set;

    while (true) {
        if (!set || it->i >= WORDBITS) {
            if (!isnextl(&set, it)) {
                it->i = 0;
                return false;
            }
        }

        if (isnext64bm(out, it)) return true;
    }
}

bool isnext(int *out, struct intset_iterator *it) {
    if (!out || !it) return false;
    int64_t i;
    bool res = isnext64(&i, it);
    *out = (int) i;
    return res;
}

void reset_isiterator(struct intset_iterator *it) {
    it->root = true;
    it->set = NULL;
    areset(it->stack);
}

// bool iselem(int k, struct intset const *set) {
// }

static struct intset *link(uint64_t kfix, struct intset *newleaf, struct intset *set) {
    struct intset *right = newleaf;
    uint64_t brm = branch_mask(kfix, set->pfix);

    if (zero(kfix, brm)) {
        right = set;
        set = newleaf;
    }

    return init_intset(mask(kfix, brm), brm, set, right);
}

static bool prefix_matches(uint64_t kfix, struct intset const *set) {
    return mask(kfix, set->mask) == set->pfix;
}

static struct intset *_isinsert(uint64_t kfix, uint64_t bitmap, struct intset *set) {
    if (!set) { // nil
        set = init_intset(kfix, bitmap, NULL, NULL);
    } else if (set->left) { // branch node
        // do the key's prefix and branch prefix match?
        if (!prefix_matches(kfix, set)) {
            set = link(kfix, init_intset(kfix, bitmap, NULL, NULL), set);
        } else if (zero(kfix, set->mask)) { // branch does not match, go left
            set->left = _isinsert(kfix, bitmap, set->left);
        } else { // branch matches, go right
            set->right = _isinsert(kfix, bitmap, set->right);
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

struct intset *isinsert(int k, struct intset *set) {
    return _isinsert(prefix(k), bitmap(k), set);
}

// void isdelete(int k, struct intset *set) {
// }
//
// bool intseteq(struct intset const *a, struct intset const *b) {
// }
//
// struct intset *isunion(struct intset *a, struct intset const *b) {
// }
//
// struct intset *isintersection(struct intset *a, struct intset const *b) {
// }
//
// bool isdisjoint(struct intset *a, struct intset const *b) {
// }
//

bool isnull(struct intset const *set) {
    return issize(set) == 0;
}

size_t issize(struct intset const *set) {
    if (!set) return 0;

    size_t n = 0;

    struct intset_iterator it;
    if (isiterator(set, &it)) {
        int64_t x;
        while (isnext64(&x, &it)) n++;
        free_isiterator(&it);
    }

    return n;
}

size_t istreesize(struct intset const *set) {
    if (!set) return 0;

    size_t n = 0;

    struct intset_iterator it;
    if (isiterator(set, &it)) {
        while (isnextn(&set, &it)) n++;
        free_isiterator(&it);
    }

    return n;
}

static size_t _istreedepth(struct intset const *set, int d) {
    if (!set) return d;
    d++;
    return max(_istreedepth(set->left, d), _istreedepth(set->right, d));
}

size_t istreedepth(struct intset const *set) {
    return _istreedepth(set, 0);
}

void print_intset(struct intset const *set) {
    if (!set) return;

    struct intset_iterator it;
    if (isiterator(set, &it)) {
        int64_t x;
        while (isnext64(&x, &it)) {
            printf("%"PRId64" ", x);
        }

        free_isiterator(&it);
    }
}

static void _print_intset_tree(struct intset const *set, int depth) {
    if (!set) return;
    indent(depth);
    printf("%s %p (%" PRIu64 ", %" PRIu64 ")\n",
            set->left ? "branch" : "leaf",
            set, set->pfix, set->mask);
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

void free_isiterator(struct intset_iterator *it) {
    if (!it) return;
    free_array(it->stack);
    it->stack = NULL;
}
