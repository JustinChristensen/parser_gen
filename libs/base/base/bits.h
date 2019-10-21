#ifndef BASE_BITS_H_
#define BASE_BITS_H_ 1

#include <stdint.h>
#include <stdbool.h>

#define BIT INT64_C(1)
#define WORDBITS 64
#define SUFFIX_MASK (0b111111)
#define PREFIX_MASK (~SUFFIX_MASK)

bool zero(int64_t i, int64_t m);
int64_t prefix(int i);
int64_t suffix(int i);
int unbitmap(int64_t bm);
int64_t bitmap(int i);
int clz(int i);
int ctz(int64_t i);
int msbpos(int i);
int64_t msbmask(int i);
int64_t branch_mask(int i, int j);
int64_t prefix_upto_branch(int i, int64_t bmask);

#endif // BASE_BITS_H_

