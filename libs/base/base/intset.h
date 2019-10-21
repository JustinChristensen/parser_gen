#ifndef BASE_INTSET_H_
#define BASE_INTSET_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "base/array.h"
#include "base/bits.h"

/**
 * https://en.wikipedia.org/wiki/Radix_tree
 *
 * Insert(key, set):
 *
 * If set is null:
 *      init leaf node with prefix(key) and bitmap(key)
 * If set is a branch node:
 *      If prefix_upto_branch_bit(key, set->branch_mask) does not equal set->prefix
 *          init leaf node with prefix(key) and bitmap(key)
 *          compute branch_mask for the new key's prefix and current set
 *          init branch node with prefix_upto_branch_bit
 *      Else If the branch bit does not match the key prefix
 *          Insert(key, set->left)
 *      Else
 *          Insert(key, set->right)
 *  If set is a leaf node:
 *      If prefix(key) == set->prefix
 *          set->bitmap |= bitmap(key)
 *      Else
 *          Init leaf node with prefix(key) and bitmap(key)
 */

#define IT_STACK_SIZE 64

struct intset {
    // everything but the trailing 6 bits
    int64_t pfix;
    // either a branch mask (branch) or a bitmap (leaf)
    // branch mask is the highest order bit where the children differ
    // bitmap is a bitmap of the trailing 6 bits of each key '|'ed together
    int64_t mask;
    // branch mask & key is zero (less)
    struct intset *left;
    // branch mask & key is one (greater)
    struct intset *right;
};

struct intset_iterator {
    // the stack
    struct array *stack;
    // are we at the root? to order negative numbers
    bool at_root;
    // cached root set for multiple iteration
    struct intset const *root;
    // current set for bitmap iteration
    struct intset const *set;
    // the bitmap iterator
    int i;
};

struct intset intset(int64_t pfix, int64_t mask, struct intset *left, struct intset *right);
struct intset *init_intset(int64_t pfix, int64_t mask, struct intset *left, struct intset *right);
bool siterator(struct intset const *set, struct intset_iterator *it);
bool snextnode(struct intset const **out, struct intset_iterator *it);
bool snextleaf(struct intset const **out, struct intset_iterator *it);
bool snextbitmap(int *out, struct intset_iterator *it);
bool snext(int *out, struct intset_iterator *it);
void reset_siterator(struct intset_iterator *it);
bool selem(int k, struct intset const *set);
struct intset *sinsert(int k, struct intset *set);
struct intset *slistinsert(int *k, size_t n, struct intset *set);
// void sdelete(int k, struct intset *set);
bool intseteq(struct intset const *s, struct intset const *t);
// struct intset *sunion(struct intset *s, struct intset const *t);
// struct intset *sintersection(struct intset *s, struct intset const *t);
// struct intset *sdifference(struct intset *s, struct intset const *t);
// bool sdisjoint(struct intset *a, struct intset const *b);
bool snull(struct intset const *a);
size_t ssize(struct intset const *set);
size_t streesize(struct intset const *set);
size_t streedepth(struct intset const *set);
void print_intset(struct intset const *set);
void print_intset_tree(struct intset const *set);
void free_siterator(struct intset_iterator *it);
void free_intset(struct intset *set);

#endif // BASE_INTSET_H_

