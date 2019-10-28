#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <base/intset.h>

int main(int argc, char *argv[]) {
    struct intset *set1 = NULL,
                  *set2 = NULL,
                  *set3 = NULL,
                  *set4 = NULL,
                  *set5 = NULL,
                  *set6 = NULL,
                  *set7 = NULL;

    for (int i = -1000; i < 1000; i += 1) {
        set1 = sinsert(i, set1);
    }

    for (int i = 700; i < 1000; i += 1) {
        set2 = sinsert(i, set2);
    }

    for (int i = -1010; i < -900; i += 1) {
        set3 = sinsert(i, set3);
    }

    for (int i = -5000; i < 5000; i += 13) {
        set6 = sinsert(i, set6);
    }

    set4 = sdifference(set1, set2);
    set5 = sdifference(set4, set3);
    set7 = sdifference(set5, set6);

    printf("set4:\n");
    print_intset(set4);
    printf("set5:\n");
    print_intset(set5);
    printf("set6:\n");
    print_intset(set6);
    printf("set7:\n");
    print_intset(set7);

    free_intset(set1);
    free_intset(set2);
    free_intset(set3);
    free_intset(set4);
    free_intset(set5);
    free_intset(set6);
    free_intset(set7);

    return 0;
}


