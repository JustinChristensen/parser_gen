#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include "base/array.h"

struct array *init_array(size_t elem_size, size_t size, bool frozen, float growth_factor) {
    size = size ? size : 1;
    frozen = frozen || false;
    growth_factor = growth_factor ? growth_factor : GROWTH_FACTOR;

    struct array *arr = malloc(sizeof *arr);
    assert(arr != NULL);
    void *buf = calloc(size, elem_size);
    assert(buf != NULL);

    *arr = (struct array) {
        .buf = buf,
        .i = 0,
        .initsize = size,
        .size = size,
        .elem_size = elem_size,
        .frozen = frozen,
        .growth_factor = growth_factor
    };

    return arr;
}

void agfactor(float growth_factor, struct array *arr) {
    assert(growth_factor > 1);
    arr->growth_factor = growth_factor;
}

void afreeze(struct array *arr) {
    arr->frozen = true;
}

void asort(int (*compare)(void const *a, void const *b), struct array *arr) {
    qsort(arr->buf, asize(arr), arr->elem_size, compare);
}

bool arrayeq(
    bool (*eleq) (void const *a, void const *b),
    struct array const *a,
    struct array const *b
) {
    size_t alen = asize(a);
    bool equal = alen == asize(b);

    if (equal) {
        for (
            size_t i = 0;
            i < alen && (equal = (*eleq)(at(i, a), at(i, b)));
            i++
        );
    }

    return equal;
}

void *at(size_t i, struct array const *arr) {
    return arr->buf + i * arr->elem_size;
}

void *atop(struct array *arr) {
    return at(arr->i - 1, arr);
}

void *abottom(struct array *arr) {
    return arr->buf;
}

void aresize(size_t size, struct array *arr) {
    if (arr->size == size) return;
#ifdef DEBUG
    printf("resizing array, old size: %ld, new size: %ld\n", arr->size, size);
#endif
    arr->buf = realloc(arr->buf, size * arr->elem_size);
    assert(arr->buf != NULL);
    if (arr->i > size) arr->i = size;
    arr->size = size;
}

static bool should_grow(struct array *arr) {
    return arr->i == arr->size;
}

static bool should_shrink(struct array *arr) {
    return arr->size > arr->initsize && arr->i / (float) arr->size < SHRINK_SIZE;
}

static void ensure_memory(struct array *arr) {
    if (!arr->frozen) {
        if (should_grow(arr)) {
            aresize(ceil(arr->size * arr->growth_factor), arr);
        } else if (should_shrink(arr)) {
            aresize(ceil(arr->size * SHRINK_FACTOR), arr);
        }
    }
}

void apush(void *elem, struct array *arr) {
    ensure_memory(arr);
    memcpy(at(arr->i, arr), elem, arr->elem_size);
    arr->i++;
}

void apop(void *out, struct array *arr) {
    ensure_memory(arr);
    arr->i--;
    memcpy(out, at(arr->i, arr), arr->elem_size);
}

size_t asize(struct array const *arr) {
    return arr->i;
}

bool aistop(void *elem, struct array *arr) {
    return atop(arr) == elem;
}

bool aempty(struct array *arr) {
    return arr->i == 0;
}

void free_array(struct array *arr) {
    free(arr->buf);
    arr->buf = NULL;
    free(arr);
}
