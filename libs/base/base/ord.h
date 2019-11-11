#ifndef BASE_ORD_H_
#define BASE_ORD_H_ 1

#include <stdlib.h>

enum ord {
    LT = -1,
    EQ = 0,
    GT = 1
};

size_t max(size_t a, size_t b);
size_t min(size_t a, size_t b);

#endif // BASE_ORD_H_

