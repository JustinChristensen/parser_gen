#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include "base/array.h"

struct array array(void *buf, size_t elem_size, int init_size, enum growth growth, float factor) {
    return (struct array) {
        .buf = buf,
        .growth = growth,
        .factor = factor,
        .i = 0,
        .init_size = init_size,
        .size = init_size,
        .elem_size = (int) elem_size,
    };
}

struct array *init_array(size_t elem_size, int init_size, enum growth growth, float factor) {
    init_size = init_size > 0 ? init_size : 1;
    if (!factor) factor = growth == LINEAR ? GROWTH_CONSTANT : GROWTH_FACTOR;

    void *buf = calloc(init_size, (int) elem_size);
    assert(buf != NULL);

    struct array *arr = malloc(sizeof *arr);
    assert(arr != NULL);

    *arr = array(buf, elem_size, init_size, growth, factor);

    return arr;
}

void agfactor(enum growth growth, float factor, struct array *arr) {
    arr->growth = growth;
    arr->factor = factor;
}

void afreeze(struct array *arr) {
    arr->growth = FROZEN;
}

void asort(int (*compare)(void const *a, void const *b), struct array *arr) {
    qsort(arr->buf, asize(arr), arr->elem_size, compare);
}

bool arrayeq(
    bool (*eleq) (void const *a, void const *b),
    struct array const *a,
    struct array const *b
) {
    int alen = asize(a);
    bool equal = alen == asize(b);

    if (equal) {
        for (
            int i = 0;
            i < alen && (equal = (*eleq)(aptr(i, a), aptr(i, b)));
            i++
        );
    }

    return equal;
}

static bool should_grow(struct array *arr) {
    return arr->i == arr->size;
}

static bool should_shrink(struct array *arr) {
    return arr->size > arr->init_size &&
        arr->i / (float) arr->size < SHRINK_SIZE;
}

static void ensure_memory(struct array *arr) {
    if (arr->growth == FROZEN) return;

    if (should_grow(arr)) {
        if (arr->growth == LINEAR) aresize(arr->size + arr->factor, arr);
        else                  aresize(ceil(arr->size * arr->factor), arr);
    } else if (should_shrink(arr)) {
        aresize(ceil(arr->size * SHRINK_FACTOR), arr);
    }
}

void aresize(int size, struct array *arr) {
    if (arr->size == size) return;
#ifdef DEBUG
    printf("resizing array, old size: %ld, new size: %ld\n", arr->size, size);
#endif
    arr->buf = realloc(arr->buf, size * arr->elem_size);
    assert(arr->buf != NULL);
    if (arr->i > size) arr->i = size;
    arr->size = size;
}

void adel(void *elem, struct array *arr) {
    if (aempty(arr)) return;

    int ei = (elem - arr->buf) / arr->elem_size;

    if (ei >= 0) {
        for (int i = ei; i < arr->i - 1 && i < arr->size; i++) {
            memcpy(aptr(i, arr), aptr(i + 1, arr), arr->elem_size);
        }

        arr->i--;
        ensure_memory(arr);
    }
}

void *aptr(int i, struct array const *arr) {
    return arr->buf + i * arr->elem_size;
}

void at(void *out, int i, struct array const *arr) {
    memcpy(out, aptr(i, arr), arr->elem_size);
}

void apush(void *elem, struct array *arr) {
    ensure_memory(arr);
    memcpy(aptr(arr->i, arr), elem, arr->elem_size);
    arr->i++;
}

void apop(void *out, struct array *arr) {
    apeek(out, arr);
    arr->i--;
    ensure_memory(arr);
}

void apeek(void *out, struct array const *arr) {
    at(out, arr->i - 1, arr);
}

void abottom(void *out, struct array const *arr) {
    at(out, 0, arr);
}

void areset(struct array *arr) {
    if (!arr) return;
    arr->i = 0;
    ensure_memory(arr);
}

int asize(struct array const *arr) {
    return arr->i;
}

bool aempty(struct array const *arr) {
    return arr->i == 0;
}

void free_array(struct array *arr) {
    free(arr->buf);
    arr->buf = NULL;
    free(arr);
}
