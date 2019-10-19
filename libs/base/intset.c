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

struct intset intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    return (struct intset) { pfix, mask, left, right };
}

struct intset *init_intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    struct intset *set = malloc(sizeof *set);
    assert(set != NULL);
    *set = intset(pfix, mask, left, right);
    return set;
}

bool isiterator(struct intset *set, struct intset_iterator *it) {
    if (!set) return false;
    it->stack = init_array(sizeof set, IT_STACK_SIZE, 0, 0);
    reset_isiterator(it);
    apush(&set, it->stack);
    return true;
}

bool isnextn(struct intset **out, struct intset_iterator *it) {
    struct array *stack = it->stack;

    if (aempty(stack)) {
        return false;
    } else {
        struct intset *set = NULL;

        apop(&set, stack);

        if (set->left) {
            if (it->root) {
                if (set->pfix > INT64_MAX) {
                    apush(&set->right, stack);
                    apush(&set->left, stack);
                } else {
                    apush(&set->left, stack);
                    apush(&set->right, stack);
                }

                it->root = false;
            } else {
                apush(&set->right, stack);
                apush(&set->left, stack);
            }
        }

        *out = set;

        return true;
    }
}

bool isnextl(int64_t *out, struct intset_iterator *it) {
    struct intset *set = it->set;

    while (true) {
        // we've gone past the edge of the bitmap
        // reset and try to get a new node
        if (!set || it->i >= WORDBITS) {
            // reset the bitmap counter
            it->i = 0;

            // find the next leaf node
            set = NULL;
            while (isnextn(&set, it)) {
                if (set->left) {
                    set = NULL;
                    continue;
                }

                break;
            }
        }

        // if we don't have a set at this point, bail
        if (!set) return false;
        it->set = set;

        // check the ith bit
        uint64_t x = set->mask & (BIT << it->i);

        // increment
        it->i++;

        // we found a set bit, return to the caller
        if (x) {
            *out = set->pfix | ctzll(x);
            return true;
        }
    }
}

bool isnexti(int *out, struct intset_iterator *it) {
    int64_t i;
    bool res = isnextl(&i, it);
    *out = (int) i;
    return res;
}

void reset_isiterator(struct intset_iterator *it) {
    it->root = true;
    it->set = NULL;
    it->i = 0;
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
// bool isnull(struct intset const *a) {
// }

// size_t issize(struct intset const *set) {}

size_t istreesize(struct intset const *set) {
    if (!set) return 0;
    struct intset_iterator it;

    if (isiterator(set, &it)) {
    }
}

static size_t _istreedepth(struct intset const *set, int d) {
    if (!set) return d;
    d++;
    return max(_istreedepth(set->left, d), _istreedepth(set->right, d));
}

size_t istreedepth(struct intset const *set) {
    return _istreedepth(set, 0);
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

void free_isiterator(struct intset_iterator *it) {
    if (!it) return;
    free_array(it->stack);
    it->stack = NULL;
}
