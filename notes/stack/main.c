#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

unsigned addn(void const *base, unsigned n) {
    void *x = &x;
    if (n == 15) return 0;

    // if (n % 10 == 0) {
        printf("stack pointer: %p, size: %ld\n", &x, base - x);
    // }

    return n + addn(base, n + 1);
}

int main(int argc, char *argv[]) {
    pthread_t me = pthread_self();
    // void const *base = pthread_get_stackaddr_np(me);

    unsigned n = addn(base, 0);
    size_t size = pthread_get_stacksize_np(me);
    printf("stack base: %p, size: %ld\n", base, size);
    printf("n: %u\n", n);

    return 0;
}
