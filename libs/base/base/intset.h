#ifndef BASE_INTSET_H_
#define BASE_INTSET_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

/*
https://en.wikipedia.org/wiki/Radix_tree

16-bit example

prefix mask
1111111111 000000

suffix mask
0000000000 111111

running example:
0000011001 111111 (1663)
0000001100 111111 (831)

From the left, find the branching mask:
0000010000 000000 (1024) branching mask
0000011001 000000 (1600)
0000001100 000000 (768)

xor: (lets us find the positions where the bits differ)
  0000011001 000000 (1600)
  0000001100 000000 (768)
= 0000010101 000000 (768)

i.e. 1600 and 768 are the same up until the 6th from the left bit

(i .&. (complement (m-1) `xor` m))

m    0000010000 000000 (1024)
i    0000011001 000000 (1600)
m-1  0000001111 111111 (1023)
~m-1 1111110000 000000 (?)
xor
m    0000010000 000000 (1024)
~m-1 1111110000 000000 (?)
x    1111100000 000000

and
i    0000011001 000000 (1600)
x    1111100000 000000
     0000000000 000000
*/

#define BIT UINT64_C(1)
#define WORDBITS 64
#define SUFFIX_MASK (0b111111)
#define PREFIX_MASK (~SUFFIX_MASK)

struct intset {
    // everything but the trailing 6 bits
    uint64_t pfix;
    // either a branch mask (branch) or a bitmap (leaf)
    // branch mask is the highest order bit where the children differ
    // bitmap is a bitmap of the trailing 6 bits of each key '|'ed together
    uint64_t mask;
    // branch mask & key is zero (less)
    struct intset *left;
    // branch mask & key is one (greater)
    struct intset *right;
};

struct intset intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right);
struct intset *init_intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right);
// bool iselem(int k, struct intset const *set);
struct intset *isinsert(int k, struct intset *set);
// void isdelete(int k, struct intset *set);
// bool intseteq(struct intset const *a, struct intset const *b);
// struct intset *isunion(struct intset *a, struct intset const *b);
// struct intset *isintersection(struct intset *a, struct intset const *b);
// bool isdisjoint(struct intset *a, struct intset const *b);
// bool isnull(struct intset const *a);
size_t issize(struct intset const *set);
size_t isnodesize(struct intset const *set);
void print_intset(struct intset const *set);
void print_intset_tree(struct intset const *set, int depth);
void free_intset(struct intset *set);

#endif // BASE_INTSET_H_

