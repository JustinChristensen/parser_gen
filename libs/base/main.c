#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <base/intset.h>
#include <base/macros.h>

int main(int argc, char *argv[]) {
    struct intset *set = NULL;

    for (int i = 0; i < 511; i++) {
        set = isinsert(i, set);
    }

    print_intset(set);

    // for (int i = 64; i < 127; i++) {
    //     set = isinsert(i, set);
    // }

    free_intset(set);

    return 0;
}
