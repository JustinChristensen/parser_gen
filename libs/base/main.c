#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include <math.h>
#include <base/intset.h>
#include <base/macros.h>

int main(int argc, char *argv[]) {
    struct intset *set = NULL;

    for (int i = 0; i < INT_MAX >> 12; i++) {
        set = isinsert(i, set);
    }

    // set = isinsert(500000, set);
    // set = isinsert(12, set);
    // set = isinsert(13000, set);
    // set = isinsert(-65566, set);
    // set = isinsert(-140000, set);

    printf("size: %ld\n", issize(set));
    printf("treesize: %ld\n", istreesize(set));
    printf("treedepth: %ld\n", istreedepth(set));
    // print_intset_tree(set);

    // struct intset_iterator it;
    // if (isiterator(set, &it)) {
    //     int i;
    //     while (isnext(&i, &it)) {
    //         printf("%d \n", i);
    //     }
    //     // iterate nodes
    //     // iterate leafs
    //         // iterate bitmap
    //     // iterate ints
    //     // iterate int64s

    //     free_isiterator(&it);
    // }

    free_intset(set);

    return 0;
}
