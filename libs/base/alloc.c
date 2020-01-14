#include <stdlib.h>
#include "base/alloc.h"

union ptr bfrom_ptr(void *ptr) { return (union ptr) { .p = ptr }; }
union ptr bfrom_int(int ptr)   { return (union ptr) { .i = ptr }; }
void *bto_ptr(union ptr p)     { return p.p; }
int bto_int(union ptr p)       { return p.i; }

static union ptr stdlib_malloc(size_t size, struct alloc *_) {
    return bfrom_ptr(malloc(size));
}

static union ptr stdlib_calloc(size_t count, size_t size, struct alloc *_) {
    return bfrom_ptr(calloc(count, size));
}

static union ptr stdlib_realloc(union ptr p, size_t size, struct alloc *_) {
    return bfrom_ptr(realloc(bto_ptr(p), size));
}

static void stdlib_free(union ptr p, struct alloc *_) {
    free(bto_ptr(p));
}

struct alloc stdlib_alloc() {
    return (struct alloc) {
        stdlib_malloc,
        stdlib_calloc,
        stdlib_realloc,
        stdlib_free
    };
}

void free_alloc(struct alloc *al) {
    if (al->free_alloc) (*al->free_alloc)(al);
}

union ptr bmalloc(size_t size, struct alloc *al) {
    return (*al->malloc)(size, al);
}

union ptr bcalloc(size_t count, size_t size, struct alloc *al) {
    return (*al->calloc)(count, size, al);
}

union ptr brealloc(union ptr p, size_t size, struct alloc *al) {
    return (*al->realloc)(p, size, al);
}

void bfree(union ptr p, struct alloc *al) {
    (*al->free)(p, al);
}

