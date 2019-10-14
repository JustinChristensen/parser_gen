#ifndef BASE_EITHER_H_
#define BASE_EITHER_H_ 1

#include <stdbool.h>

struct either {
    void *left;
    void *right;
};

struct either either(void *left, void *right);
bool is_left(struct either either);
bool is_right(struct either either);

#endif // BASE_EITHER_H_
