#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include <math.h>
#include <base/intset.h>
#include <base/macros.h>

int main(int argc, char *argv[]) {
    struct intset *set = NULL;

    for (int i = 0; i < INT_MAX >> 8; i++) {
        set = isinsert(i, set);
    }

    // for (int i = 0; i < 128; i += 3) {
    //     set = isinsert(i, set);
    // }

    // for (int i = 0; i >= -256; i -= 2) {
    //     set = isinsert(i, set);
    // }

    // set = isinsert(500000, set);
    // set = isinsert(12, set);
    // set = isinsert(13000, set);
    // set = isinsert(-65566, set);

    printf("size: %ld\n", istreesize(set));
    printf("depth: %ld\n", istreedepth(set));

    // print_intset_tree(set, 0);

    struct intset_iterator it;
    if (isiterator(set, &it)) {
        int i;
        while (isnexti(&i, &it)) {
            // printf("%d\n", i);
        }

        free_isiterator(&it);
    }

    free_intset(set);

    return 0;
}
