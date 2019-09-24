#include "base/either.h"

struct either either(void *left, void *right) {
    return (struct either) { left, right };
}

bool is_left(struct either either) {
    return either.left != NULL;
}

bool is_right(struct either either) {
    return either.right != NULL;
}
