#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <base/intset.h>

int main(int argc, char *argv[]) {
    struct intset *set = NULL;

    for (int i = 1; i < argc; i++) {
        set = sinsert(atoi(argv[i]), set);
    }

    print_intset_tree(set);

    free_intset(set);

    return 0;
}
