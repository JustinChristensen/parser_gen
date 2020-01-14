#ifndef BASE_ALLOC_H_
#define BASE_ALLOC_H_ 1

#include <stdlib.h>

union ptr {
    void *p;
    int i;
};

#endif // BASE_ALLOC_H_
