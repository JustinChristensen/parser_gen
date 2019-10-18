#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include "base/intset.h"
#include "base/string.h"

static bool zero(uint64_t i, uint64_t m) {
    return (i & m) == 0;
}

// leading word - 6 bits
static uint64_t prefix(int i) {
    return i & PREFIX_MASK;
}

// trailing 6 bits
static uint64_t suffix(int i) {
    return i & SUFFIX_MASK;
}

// 001 (0), 010 (1), 100 (2), etc...
static uint64_t bitmap(int i) {
    return BIT << suffix(i);
}

// builtins are undefined for 0
static int clzll(int i) {
    return i ? __builtin_clzll(i) : WORDBITS;
}

// used to map the leaf node bitmap back to integer values
static int ctzll(uint64_t i) {
    return i ? __builtin_ctzll(i) : WORDBITS;
}

// most significant bit position
static int msbpos(int i) {
    return (WORDBITS - 1) - clzll(i);
}

// most significant bit mask
static uint64_t msbmask(int i) {
    return BIT << msbpos(i);
}

// find the bit position where i and j differ by first
// xoring them together to identify the differing bits
// then finding the most significant differing bit
// then create a bit mask
static uint64_t branch_mask(int i, int j) {
    return msbmask(i ^ j);
}

// get the leading bits of the key up to the branch bit
static uint64_t mask(int i, uint64_t bmask) {
    return i & (~(bmask - 1) ^ bmask);
}

static bool prefix_matches(uint64_t kfix, struct intset const *set) {
    return mask(kfix, set->mask) == set->pfix;
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

static struct intset *_isinsert(uint64_t kfix, uint64_t bitmap, struct intset *set) {
    if (!set) { // nil
        set = init_intset(kfix, bitmap, NULL, NULL);
    } else if (set->left) { // branch node
        // do the key's prefix and branch prefix match?
        if (!prefix_matches(kfix, set)) {
            set = link(kfix, init_intset(kfix, bitmap, NULL, NULL), set);
        } else if (zero(kfix, set->mask)) { // less than, go left
            set->left = _isinsert(kfix, bitmap, set->left);
        } else { // otherwise, go right
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
// bool isnull(struct intset const *a) {
// }

// size_t issize(struct intset const *set) {}

static size_t _isnodesize(struct intset const *set, int n) {
    if (!set) return n;
    n++;
    n = _isnodesize(set->left, n);
    return _isnodesize(set->right, n);
}

size_t isnodesize(struct intset const *set) {
    return _isnodesize(set, 0);
}

static void _print_intset(struct intset const *set) {
    if (!set) return;

    if (set->left) {
        _print_intset(set->left);
        _print_intset(set->right);
    } else {
        uint64_t x;
        for (int i = 0; i < WORDBITS; i++) {
            x = set->mask & (BIT << i);
            if (x) printf("%"PRId64" ", set->pfix | ctzll(x));
        }
    }
}

void print_intset(struct intset const *set) {
    if (!set) return;

    if (set->pfix > INT64_MAX) {
        _print_intset(set->left);
        _print_intset(set->right);
    } else {
        _print_intset(set->right);
        _print_intset(set->left);
    }
}

void print_intset_tree(struct intset const *set, int depth) {
    if (!set) return;
    indent(depth);
    printf("%s %p (%" PRIu64 ", %" PRIu64 ")\n",
            set->left ? "branch" : "leaf",
            set, set->pfix, set->mask);
    depth++;
    print_intset_tree(set->left, depth);
    print_intset_tree(set->right, depth);
}

void free_intset(struct intset *set) {
    if (!set) return;
    free_intset(set->left);
    free_intset(set->right);
    free(set);
}
