#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "base/bits.h"

bool zero(int64_t i, int64_t m) {
    return (i & m) == 0;
}

// leading word - 6 bits
int64_t prefix(int i) {
    return i & PREFIX_MASK;
}

// trailing 6 bits
int64_t suffix(int i) {
    return i & SUFFIX_MASK;
}

// 001 (0), 010 (1), 100 (2), etc...
int64_t bitmap(int i) {
    return BIT << suffix(i);
}

int unbitmap(int64_t bm) {
    return ctz(bm);
}

// builtins are undefined for 0
int clz(int i) {
    return i ? __builtin_clzll(i) : WORDBITS;
}

// used to map the leaf node bitmap back to integer values
int ctz(int64_t i) {
    return i ? __builtin_ctzll(i) : WORDBITS;
}

// most significant bit position
int msbpos(int i) {
    return (WORDBITS - 1) - clz(i);
}

// most significant bit mask
int64_t msbmask(int i) {
    return BIT << msbpos(i);
}

// find the bit position where i and j differ by first
// xoring them together to identify the differing bits
// then finding the most significant differing bit
// then create a bit mask
int64_t branch_mask(int i, int j) {
    return msbmask(i ^ j);
}

// get the leading bits of the key up to the branch bit
int64_t prefix_upto_branch(int i, int64_t bmask) {
    return i & (~(bmask - 1) ^ bmask);
}

void printbits(int64_t x) {
    for (int i = WORDBITS - 1; i >= 0; i--) {
        printf("%c", x & (BIT << i) ? '1' : '0');
    }
}
