#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include <string.h>
#include <assert.h>
#include "base/bitset.h"

#define WORDBITS (sizeof (size_t) * CHAR_BIT)
#define WINDEX(i) (i / WORDBITS)
#define BINDEX(i) (i % WORDBITS)
#define BIT (1UL)
#define MASK(i) (BIT << BINDEX(i))

struct bitset *bitset(unsigned n) {
    size_t nwords = WINDEX(n + WORDBITS - 1);
    size_t wbytes = nwords * sizeof (size_t);
    struct bitset *bitset = malloc(sizeof *bitset + wbytes);
    if (!bitset) return NULL;
    bitset->nwords = nwords;
    memset(bitset->words, 0, wbytes);
    return bitset;
}

bool bselem(unsigned i, struct bitset const *s) {
    assert(s != NULL);
    return s->words[WINDEX(i)] & MASK(i);
}

void bsins(unsigned i, struct bitset *s) {
    assert(s != NULL);
    s->words[WINDEX(i)] |= MASK(i);
}

void bsdel(unsigned i, struct bitset *s) {
    assert(s != NULL);
    s->words[WINDEX(i)] &= ~MASK(i);
}

void bsunion(struct bitset *s, struct bitset const *t) {
    assert(s->nwords == t->nwords);
    for (int w = 0; w < s->nwords; w++)
        s->words[w] |= t->words[w];
}

void bsintersect(struct bitset *s, struct bitset const *t) {
    assert(s->nwords == t->nwords);
    for (int w = 0; w < s->nwords; w++)
        s->words[w] &= t->words[w];
}

bool bsdisjoint(struct bitset const *s, struct bitset const *t) {
    assert(s->nwords == t->nwords);
    for (int w = 0; w < s->nwords; w++)
        if (s->words[w] & t->words[w]) return false;
    return true;
}

unsigned bssize(struct bitset const *s) {
    assert(s != NULL);

    unsigned n = 0;

    for (int w = 0; w < s->nwords; w++)
        for (int i = 0; i < WORDBITS; i++)
            if (s->words[w] & (BIT << i)) n++;

    return n;
}

void print_bitset(FILE *handle, struct bitset const *s) {
    assert(s != NULL);

    for (int w = 0; w < s->nwords; w++)
        for (int i = 0; i < WORDBITS; i++)
            if (s->words[w] & (BIT << i))
                fprintf(handle, "%u ", (unsigned) (w * WORDBITS + 1));
}

void print_bitset_bits(FILE *handle, struct bitset const *s) {
    assert(s != NULL);

    for (int w = s->nwords - 1; w >= 0; w--) {
        for (int i = WORDBITS - 1; i >= 0; i--)
            fprintf(handle, "%u", (s->words[w] & (BIT << i)) ? 1 : 0);
        fprintf(handle, " ");
    }
}

struct bitset_iter bitset_iter(struct bitset *s) {
    assert(s != NULL);
    return (struct bitset_iter) { s };
}

bool bsnext(unsigned *i, struct bitset_iter *it) {
    assert(i != NULL);
    assert(it != NULL);

    struct bitset const *set = it->set;
    unsigned w = it->w;
    unsigned n = it->n;

    for (; w < set->nwords; w++) {
        for (; n < WORDBITS; n++) {
            if (set->words[w] & (1 << n)) {
                *i = (WORDBITS * w) + n++;
                it->w = w;
                it->n = n;
                return true;
            }
        }
    }

    it->w = it->n = 0;

    return false;
}


