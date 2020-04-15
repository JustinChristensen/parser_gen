#ifndef BASE_BITSET_H_
#define BASE_BITSET_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

struct bitset {
    unsigned nwords;
    size_t words[];
};

struct bitset_iter {
    struct bitset *set;
    unsigned w;
    unsigned n;
};

struct bitset *bitset(unsigned n);
bool bselem(unsigned i, struct bitset const *s);
void bsins(unsigned i, struct bitset *s);
void bsdel(unsigned i, struct bitset *s);
void bsunion(struct bitset *s, struct bitset const *t);
void bsintersect(struct bitset *s, struct bitset const *t);
bool bsdisjoint(struct bitset const *s, struct bitset const *t);
unsigned bssize(struct bitset const *s);
void print_bitset(FILE *handle, struct bitset const *s);
void print_bitset_bits(FILE *handle, struct bitset const *s);
struct bitset_iter bitset_iter(struct bitset *s);
bool bsnext(unsigned *i, struct bitset_iter *it);

#endif // BASE_BITSET_H_
