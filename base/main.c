#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <base/stack.h>

int main(int argc, char *argv[]) {
    printf("sizeof(long double): %ld\n", sizeof(long double));
    printf("sizeof(struct stack): %ld\n", sizeof(struct stack));
    struct stack *st = init_stack(sizeof(long double), 100, 0);
    for (int i = 0; i < 100000000; i++) {
        long double x = i;
        push(&x, st);
    }
    while (!sempty(st)) {
        pop(st);
    }
    free_stack(st);

    return 0;
}
