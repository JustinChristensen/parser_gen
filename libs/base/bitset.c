#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <limits.h>
#include <string.h>
#include <assert.h>
#include "base/bitset.h"

#define WORDBITS (sizeof (size_t) * CHAR_BIT)
#define WINDEX(i) ((i) / WORDBITS)
#define BINDEX(i) ((i) % WORDBITS)
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

void bsinsarr(struct bitset *s, unsigned n, unsigned *arr) {
    assert(s != NULL);
    for (unsigned i = 0; i < n; i++) bsins(arr[i], s);
}

void bsdel(unsigned i, struct bitset *s) {
    assert(s != NULL);
    s->words[WINDEX(i)] &= ~MASK(i);
}

#define SETOP(name, op) \
static void vbs##name##_(struct bitset *s, va_list args) { \
    assert(s != NULL);                                     \
    struct bitset *t = NULL;                               \
    while ((t = va_arg(args, struct bitset *))) {          \
        assert(s->nwords == t->nwords);                    \
        for (int w = 0; w < s->nwords; w++)                \
            s->words[w] op t->words[w];                    \
    }                                                      \
}                                                          \
                                                           \
void bs##name##_(struct bitset *s, ...) {                  \
    va_list args;                                          \
    va_start(args, s);                                     \
    vbs##name##_(s, args);                                 \
    va_end(args);                                          \
}

SETOP(union, |= )

SETOP(intersect, &= )

SETOP(difference, &= ~)

bool bsdisjoint(struct bitset const *s, struct bitset const *t) {
    assert(s != NULL);
    assert(s->nwords == t->nwords);

    for (int w = 0; w < s->nwords; w++)
        if (s->words[w] & t->words[w]) return false;

    return true;
}

bool bseq(struct bitset const *s, struct bitset const *t) {
    assert(s != NULL);
    assert(t != NULL);
    assert(s->nwords == t->nwords);
    for (int w = 0; w < s->nwords; w++)
        if (s->words[w] != t->words[w]) return false;
    return true;
}

bool bsnull(struct bitset const *s) {
    assert(s != NULL);
    for (int w = 0; w < s->nwords; w++)
        if (s->words[w] != 0) return false;
    return true;
}

void bszero(struct bitset *s) {
    assert(s != NULL);
    memset(s->words, 0, s->nwords * sizeof *s->words);
}

void bszeron(unsigned n, struct bitset *s) {
    assert(s != NULL);

    unsigned nwords = WINDEX(n), rem = BINDEX(n);
    assert(nwords < s->nwords || nwords == s->nwords && !rem);

    memset(s->words, 0, nwords * sizeof *s->words);
    if (rem) s->words[nwords] &= ~((BIT << rem) - 1);
}

unsigned bssize(struct bitset const *s) {
    assert(s != NULL);

    unsigned n = 0;

    for (unsigned w = 0; w < s->nwords; w++)
        for (unsigned i = 0; i < WORDBITS; i++)
            if (s->words[w] & (BIT << i)) n++;

    return n;
}

void print_bitset(FILE *handle, struct bitset const *s) {
    assert(s != NULL);

    for (unsigned w = 0; w < s->nwords; w++)
        for (unsigned i = 0; i < WORDBITS; i++)
            if (s->words[w] & (BIT << i))
                fprintf(handle, "%u ", (unsigned) (w * WORDBITS + i));
}

void print_bitset_bits(FILE *handle, struct bitset const *s) {
    assert(s != NULL);

    for (int w = s->nwords - 1; w >= 0; w--) {
        for (int i = WORDBITS - 1; i >= 0; i--)
            fprintf(handle, "%u", (s->words[w] & (BIT << i)) ? 1 : 0);
        fprintf(handle, " ");
    }
}

struct bsiter bsiter(struct bitset *s) {
    assert(s != NULL);
    return (struct bsiter) { s };
}

bool bsnext(unsigned *i, struct bsiter *it) {
    assert(i != NULL);
    assert(it != NULL);

    struct bitset const *set = it->set;
    unsigned w = it->w;
    unsigned n = it->n;

    for (; w < set->nwords; n = 0, w++) {
        for (; n < WORDBITS; n++) {
            if (set->words[w] & (BIT << n)) {
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


