#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include "base/array.h"

struct array *init_array(size_t elem_size, size_t size, float growth_factor) {
    size = size ? size : 1;
    growth_factor = growth_factor ? growth_factor : GROWTH_FACTOR;
    struct array *array = malloc(sizeof *array);
    assert(array != NULL);
    void *buf = calloc(size, elem_size);
    assert(buf != NULL);
    *array = (struct array) { buf, 0, size, size, elem_size, growth_factor };
    return array;
}

void *at(size_t i, struct array *array) {
    return array->buf + i * array->elem_size;
}

void *atop(struct array *array) {
    return at(array->i - 1, array);
}

void *abottom(struct array *array) {
    return array->buf;
}

void aresize(size_t size, struct array *array) {
    if (array->size == size) return;
#ifdef DEBUG
    printf("resizing array, old size: %ld, new size: %ld\n", array->size, size);
#endif
    array->buf = realloc(array->buf, size * array->elem_size);
    assert(array->buf != NULL);
    if (array->i > size) array->i = size;
    array->size = size;
}

static bool should_grow(struct array *array) {
    return array->i == array->size;
}

static bool should_shrink(struct array *array) {
    return array->size > array->initsize && array->i / (float) array->size < SHRINK_SIZE;
}

static void ensure_memory(struct array *array) {
    if (should_grow(array)) {
        aresize(ceil(array->size * array->growth_factor), array);
    } else if (should_shrink(array)) {
        aresize(ceil(array->size * SHRINK_FACTOR), array);
    }
}

void apush(void *elem, struct array *array) {
    ensure_memory(array);
    memcpy(at(array->i, array), elem, array->elem_size);
    array->i++;
}

void apop(void *out, struct array *array) {
    ensure_memory(array);
    array->i--;
    memcpy(out, at(array->i, array), array->elem_size);
}

size_t asize(struct array *array) {
    return array->i;
}

bool aistop(void *elem, struct array *array) {
    return atop(array) == elem;
}

bool aempty(struct array *array) {
    return array->i == 0;
}

void free_array(struct array *array) {
    free(array->buf);
    array->buf = NULL;
    free(array);
}
