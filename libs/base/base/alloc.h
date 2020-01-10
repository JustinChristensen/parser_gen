#ifndef BASE_ALLOC_H_
#define BASE_ALLOC_H_ 1

#include <stdlib.h>

union ptr {
    void *p;
    int i;
};

struct balloc {
    void *buf;
};

struct ptr bmalloc(size_t size);
void bfree(union ptr ptr);

#endif // BASE_ALLOC_H_
