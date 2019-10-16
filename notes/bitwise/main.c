#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define BIT ((uint64_t)1)
#define SUFFIX_MASK (0b111111)
#define PREFIX_MASK (~SUFFIX_MASK)

uint64_t prefix(int k) {
    return k & PREFIX_MASK;
}

uint64_t suffix(int k) {
    return k & SUFFIX_MASK;
}

uint64_t bitmap(int k) {
    return BIT << suffix(k);
}

// most significant bit position
int msbpos(int i) {
    return (64 - 1) - __builtin_clzll(i);
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
    printf("prefix: %llu, suffix: %llu\n", prefix(1663), suffix(1663));
    printf("prefix: %llu, suffix: %llu\n", prefix(831), suffix(831));
    printf("bitmap: %llu\n", bitmap(1663));
    printf("bitmap: %llu\n", bitmap(831));

    int bpos = 32 - 1 - __builtin_clz(1663);
    printf("32: 1663 clz: %d, bpos : %d\n", __builtin_clz(1663), bpos);
    bpos = 64 - 1 - __builtin_clzll(1663);
    printf("64: 1663 clz: %d, bpos : %d\n", __builtin_clzll(1663), bpos);

    printf("branch_mask: %llu\n", branch_mask(1663, 768));

    uint64_t p, n, b, m;
    p = n = b = m = 0;
    for (int i = 0; i < 511; i++) {
        printf("p: %llu, b: %llu, m: %llu\n", p, b, m);

        n = prefix(i);
        b = branch_mask(p, n);
        m = mask(n, b);
        p = n;
    }

    // int x = 0b101011000011; // 2755
    // int y = 0b000000000011; // 3
    // printf("x: %d, y: %d\n", x, y);
    // printf("x: %d, x & ~y: %d\n", x, x & ~y);

    // int t = 0;
    // for (int i = 0; i < 15; i++) {
    //     printf("%d ", t);
    //     t |= (1 << i);
    // }
    // printf("\n");

    // int z = 255;
    // unsigned char *zp = (unsigned char *) &z;

    // printf("%p %p\n", zp, (zp + 3));
    // if ((int) *zp == 255) {
    //     printf("little endian\n");
    // } else if ((int) *(zp + 3) == 255) {
    //     printf("big endian\n");
    // }

    // int a = 0b11100111111;
    // int b = 0b10100111111;

    // printf("%d %d\n", a, b);

    return 0;
}
