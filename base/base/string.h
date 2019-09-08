#ifndef BASE_STRING_H_
#define BASE_STRING_H_ 1

#include <stdlib.h>

struct sizenl {
    size_t sl;
    size_t nl;
};

struct sizenl numlines(char *str);

#endif // BASE_STRING_H_
