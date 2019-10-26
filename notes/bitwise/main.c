#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

#define BIT ((uint64_t)1)
#define SUFFIX_MASK (0b111111)
#define PREFIX_MASK (~SUFFIX_MASK)

uint64_t prefix(int k) {
    return k & PREFIX_MASK;
}

uint64_t suffix(int k) {
    return k & SUFFIX_MASK;
}

int64_t prefix2(int k) {
    return k & PREFIX_MASK;
}

int64_t suffix2(int k) {
    return k & SUFFIX_MASK;
}

uint64_t bitmap(int k) {
    return BIT << suffix(k);
}

// __builtin_clz is undefined for 0
int clz(int i) {
    return i ? __builtin_clzll(i) : 64;
}

int ctzll(uint64_t i) {
    return i ? __builtin_ctzll(i) : 64;
}

// most significant bit position
int msbpos(int i) {
    return (64 - 1) - clz(i);
}

// most significant bit mask
uint64_t msbmask(int i) {
    return BIT << msbpos(i);
}

uint64_t branch_mask(int i, int j) {
    return msbmask(i ^ j);
}

uint64_t mask(int i, uint64_t m) {
    return i & (~(m - 1) ^ m);
}

int main(int argc, char *argv[]) {
    printf("prefix: %"PRIu64", suffix: %"PRIu64"\n", prefix(1663), suffix(1663));
    printf("prefix: %"PRIu64", suffix: %"PRIu64"\n", prefix(831), suffix(831));
    printf("bitmap: %"PRIu64"\n", bitmap(1663));
    printf("bitmap: %"PRIu64"\n", bitmap(831));
    printf("bitmap: %"PRIu64"\n", bitmap(0));

    int bpos = 32 - 1 - __builtin_clz(1663);
    printf("32: 1663 clz: %d, bpos : %d\n", __builtin_clz(1663), bpos);
    bpos = 64 - 1 - __builtin_clzll(1663);
    printf("64: 1663 clz: %d, bpos : %d\n", __builtin_clzll(1663), bpos);

    printf("branch_mask: %"PRIu64"\n", branch_mask(1663, 768));

    printf("prefix: %"PRIu64"\n", prefix(-1));

    for (int i = 0; i < 64; i++) {
        printf("prefix2: %"PRId64"\n", prefix2(-i));
    }

    printf("uint64 to int64 cast: %"PRId64"\n", (int64_t) 0b1111111111111111111111111111111111111111111111111111111100000000);
    printf("__builtin_clzll(0) %d\n", __builtin_clzll(0));

    uint64_t pfix = UINT64_C(18446744073709551552);
    uint64_t bitmap = 0b1111111111111111111111111111111111111111111111111111111111111111;

    for (int i = 0; i < 64; i++) {
        uint64_t x = (UINT64_C(1) << i) & bitmap;
        if (x) {
            printf("%"PRId64" ", pfix | ctzll(x));
        }
    }
    printf("\n");

    printf("pfix is negative?: %s\n", (int64_t) pfix < 0 ? "yes" : "no");

    printf("%"PRId64" ", UINT64_C(18446744073709551360));
    printf("%"PRId64" ", UINT64_C(18446744073709551424));
    printf("%"PRId64" ", UINT64_C(18446744073709551488));
    printf("%"PRId64" ", UINT64_C(18446744073709551552));

    printf("\n%"PRId64"\n", UINT64_C(0b1111111111111111111111111111111111111111111111110011110000000000));

    return 0;
}
