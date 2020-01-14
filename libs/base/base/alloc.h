#ifndef BASE_ALLOC_H_
#define BASE_ALLOC_H_ 1

#include <stdlib.h>
#include "base/array.h"

union ptr {
    void *p;
    int i;
};

struct alloc {
    union ptr (*malloc)(size_t size, struct alloc *al);
    union ptr (*calloc)(size_t count, size_t size, struct alloc *al);
    union ptr (*realloc)(union ptr p, size_t size, struct alloc *al);
    void (*free)(union ptr p, struct alloc *al);
    void (*free_alloc)(struct alloc *al);
    // union {
    //     struct array *arr;
    // };
};

struct alloc stdlib_alloc();
// struct alloc array_alloc();
void free_alloc(struct alloc *al);

union ptr bfrom_ptr(void *ptr);
union ptr bfrom_int(int ptr);
void *bto_ptr(union ptr p);
int bto_int(union ptr p);

union ptr bmalloc(size_t size, struct alloc *al);
union ptr bcalloc(size_t count, size_t size, struct alloc *al);
union ptr brealloc(union ptr p, size_t size, struct alloc *al);
void bfree(union ptr p, struct alloc *al);

#endif // BASE_ALLOC_H_
