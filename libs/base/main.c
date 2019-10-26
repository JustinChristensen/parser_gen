#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <base/intset.h>

int main(int argc, char *argv[]) {
    struct intset *set1 = NULL,
                  *set2 = NULL,
                  *set3 = NULL,
                  *set4 = NULL,
                  *set5 = NULL;

    for (int i = -1000; i < -500; i += 1) {
        set1 = sinsert(i, set1);
    }

    for (int i = 500; i < 1000; i += 1) {
        set2 = sinsert(i, set2);
    }

    set3 = sunion(set1, set2);

    printf("union:\n");
    print_intset(set3);
    printf("\n");

    for (int i = 500; i < 1000; i += 3) {
        set4 = sinsert(i, set4);
    }

    set5 = sintersection(set3, set4);

    printf("set4: %ld\n", ssize(set4));
    printf("intersection:\n");
    print_intset(set5);
    printf("\n");
    print_intset_tree(set4);
    print_intset_tree(set5);

    free_intset(set1);
    free_intset(set2);
    free_intset(set3);
    free_intset(set4);
    free_intset(set5);

    return 0;
}


