#ifndef BASE_BITS_H_
#define BASE_BITS_H_ 1

#include <stdint.h>
#include <stdbool.h>

#define BIT UINT64_C(1)
#define WORDBITS 64
#define SUFFIX_MASK (0b111111)
#define PREFIX_MASK (~SUFFIX_MASK)

bool zero(uint64_t i, uint64_t m);
uint64_t prefix(int i);
uint64_t suffix(int i);
uint64_t bitmap(int i);
int clzll(int i);
int ctzll(uint64_t i);
int msbpos(int i);
uint64_t msbmask(int i);
uint64_t branch_mask(int i, int j);
uint64_t mask(int i, uint64_t bmask);

#endif // BASE_BITS_H_

