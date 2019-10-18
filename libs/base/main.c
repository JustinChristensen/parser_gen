#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include <math.h>
#include <base/intset.h>
#include <base/macros.h>

int main(int argc, char *argv[]) {
    struct intset *set = NULL;

    for (int i = 0; i < 128; i++) {
        set = isinsert(i, set);
    }

    for (int i = 0; i >= -256; i--) {
        set = isinsert(i, set);
    }

    // set = isinsert(500000, set);
    // set = isinsert(12, set);
    // set = isinsert(13000, set);
    // set = isinsert(-65566, set);

    printf("size: %ld\n", isnodesize(set));
    print_intset_tree(set, 0);

    print_intset(set);
    printf("\n");

    free_intset(set);

    return 0;
}
