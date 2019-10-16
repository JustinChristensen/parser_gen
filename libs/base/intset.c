#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include "base/intset.h"

static bool left(uint64_t i, uint64_t m) {
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

// 000 (0), 001 (1), 010 (2), 100 (3), etc...
static uint64_t bitmap(int i) {
    return BIT << suffix(i);
}

// most significant bit position
static int msbpos(int i) {
    return (64 - 1) - __builtin_clzll(i);
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

// i should read the paper...
static uint64_t mask(int i, uint64_t m) {
    return i & (~(m - 1) ^ m);
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

    if (left(kfix, brm)) {
        right = set;
        set = newleaf;
    }

    return init_intset(mask(kfix, brm), brm, set, right);
}

static struct intset *_isinsert(uint64_t kfix, uint64_t bm, struct intset *set) {
    if (!set) { // nil
        set = init_intset(kfix, bm, NULL, NULL);
    } else if (set->left) { // branch node
        if (mask(kfix, set->mask) != set->pfix) {
            set = link(kfix, init_intset(kfix, bm, NULL, NULL), set);
        } else if (left(kfix, set->mask)) {
            set->left = _isinsert(kfix, bm, set->left);
        } else {
            set->right = _isinsert(kfix, bm, set->right);
        }
    } else  { // leaf node
        if (set->pfix == kfix) {
            set->mask |= bm;
        } else {
            set = link(kfix, init_intset(kfix, bm, NULL, NULL), set);
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
//
// size_t issize(struct intset const *a) {
// }

void print_intset(struct intset const *set) {
    if (!set) return;
    printf("%s %p (%" PRIu64 ", %" PRIu64 ")\n",
            set->left ? "branch" : "leaf",
            set, set->pfix, set->mask);
    print_intset(set->left);
    print_intset(set->right);
}


void free_intset(struct intset *set) {
    if (!set) return;
    free_intset(set->left);
    free_intset(set->right);
    free(set);
}


