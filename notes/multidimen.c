#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned int **alloc() {
    unsigned int const x = 5;
    unsigned int const y = 6;
    unsigned int **ns = malloc(x * sizeof *ns + x * y * sizeof **ns);
    unsigned int *n = (unsigned int *) (ns + x);

    unsigned int *rs[] = {
        (unsigned int[]) { 45, 55, 65, 75, 85, 0 },
        (unsigned int[]) { 3, 6, 9, 12, 15, 0 },
        (unsigned int[]) { 46, 12, 31, 27, 27, 0 },
        (unsigned int[]) { 910, 900, 890, 870, 810, 0 },
        (unsigned int[]) { 333, 444, 555, 666, 777, 0 }
    };

    for (int i = 0; i < x; i++) {
        ns[i] = n;
        memcpy(n, rs[i], y * sizeof (unsigned int));
        n += y;
    }

    return ns;
}

int main(int argc, char *argv[]) {
    unsigned int **ns = alloc();
    for (int i = 0; i < 5; i++) {
        unsigned int *n = ns[i];
        while (*n) printf("%d ", *n), n++;
        printf("\n");
    }
    free(ns);
    return 0;
}
