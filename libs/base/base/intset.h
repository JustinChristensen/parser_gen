#ifndef BASE_INTSET_H_
#define BASE_INTSET_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "base/array.h"
#include "base/bits.h"

/*
https://en.wikipedia.org/wiki/Radix_tree
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
    bool root;
    // current set for bitmap iteration
    struct intset *set;
    // the bitmap iterator
    int i;
};

struct intset intset(int64_t pfix, int64_t mask, struct intset *left, struct intset *right);
struct intset *init_intset(int64_t pfix, int64_t mask, struct intset *left, struct intset *right);
bool isiterator(struct intset const *set, struct intset_iterator *it);
bool isnextn(struct intset const **out, struct intset_iterator *it);
bool isnextl(struct intset const **out, struct intset_iterator *it);
bool isnextbm(int *out, struct intset_iterator *it);
bool isnext(int *out, struct intset_iterator *it);
void reset_isiterator(struct intset_iterator *it);
// bool iselem(int k, struct intset const *set);
struct intset *isinsert(int k, struct intset *set);
// void isdelete(int k, struct intset *set);
// bool intseteq(struct intset const *a, struct intset const *b);
// struct intset *isunion(struct intset *a, struct intset const *b);
// struct intset *isintersection(struct intset *a, struct intset const *b);
// bool isdisjoint(struct intset *a, struct intset const *b);
bool isnull(struct intset const *a);
size_t issize(struct intset const *set);
size_t istreesize(struct intset const *set);
size_t istreedepth(struct intset const *set);
void print_intset(struct intset const *set);
void print_intset_tree(struct intset const *set);
void free_isiterator(struct intset_iterator *it);
void free_intset(struct intset *set);

#endif // BASE_INTSET_H_

