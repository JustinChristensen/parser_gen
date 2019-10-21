#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <base/intset.h>

int main(int argc, char *argv[]) {
    struct intset *set = NULL;

    for (int i = -100000000; i < 100000000; i+=2) {
        set = sinsert(i, set);
    }

    printf("%lu\n", ssize(set));

    free_intset(set);

    return 0;
}
