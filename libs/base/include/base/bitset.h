#ifndef BASE_BITSET_H_
#define BASE_BITSET_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

struct bitset {
    unsigned short nwords;
    size_t words[];
};

struct bsiter {
    struct bitset *set;
    unsigned w;
    unsigned n;
};

struct bitset *bitset(unsigned n);
bool bselem(unsigned i, struct bitset const *s);
void bsins(unsigned i, struct bitset *s);
void bsinsarr(struct bitset *s, unsigned n, unsigned *arr);
void bsdel(unsigned i, struct bitset *s);
#define bsunion(s, ...) bsunion_((s), __VA_ARGS__, NULL)
void bsunion_(struct bitset *s, ...);
#define bsintersect(s, ...) bsintersect_((s), __VA_ARGS__, NULL)
void bsintersect_(struct bitset *s, ...);
#define bsdifference(s, ...) bsdifference_((s), __VA_ARGS__, NULL)
void bsdifference_(struct bitset *s, ...);
bool bsdisjoint(struct bitset const *s, struct bitset const *t);
bool bseq(struct bitset const *s, struct bitset const *t);
bool bsnull(struct bitset const *s);
void bszero(struct bitset *s);
unsigned bssize(struct bitset const *s);
void print_bitset(FILE *handle, struct bitset const *s);
void print_bitset_bits(FILE *handle, struct bitset const *s);
struct bsiter bsiter(struct bitset *s);
bool bsnext(unsigned *i, struct bsiter *it);

#endif // BASE_BITSET_H_
