#ifndef BASE_ARRAY_H_
#define BASE_ARRAY_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/tuple.h"

struct array {
    size_t size;
    void *arr;
};

struct array array(size_t size, void *arr);
struct tuple2 partition(void *arr, size_t size, bool (*predicate) (void *el));
void *first(struct array arr);
size_t size(struct array arr);

#endif // BASE_ARRAY_H_
