#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include "base/stack.h"

struct stack *init_stack(size_t elem_size, size_t size, float growth_factor) {
    size = size ? size : 1;
    growth_factor = growth_factor ? growth_factor : GROWTH_FACTOR;
    struct stack *stack = malloc(sizeof *stack);
    assert(stack != NULL);
    void *buf = calloc(size, elem_size);
    assert(buf != NULL);
    *stack = (struct stack) { buf, 0, size, size, elem_size, growth_factor };
    return stack;
}

void *at(size_t i, struct stack *stack) {
    return stack->buf + i * stack->elem_size;
}

void *top(struct stack *stack) {
    return at(stack->i - 1, stack);
}

void *bottom(struct stack *stack) {
    return stack->buf;
}

void resize(size_t size, struct stack *stack) {
    if (stack->size == size) return;
#ifdef DEBUG
    printf("resizing stack, old size: %ld, new size: %ld\n", stack->size, size);
#endif
    stack->buf = realloc(stack->buf, size * stack->elem_size);
    assert(stack->buf != NULL);
    if (stack->i > size) stack->i = size;
    stack->size = size;
}

static bool should_grow(struct stack *stack) {
    return stack->i == stack->size;
}

static bool should_shrink(struct stack *stack) {
    return stack->size > stack->isize && stack->i / (float) stack->size < SHRINK_SIZE;
}

static void ensure_memory(struct stack *stack) {
    if (should_grow(stack)) {
        resize(ceil(stack->size * stack->growth_factor), stack);
    } else if (should_shrink(stack)) {
        resize(ceil(stack->size * SHRINK_FACTOR), stack);
    }
}

void spush(void *elem, struct stack *stack) {
    ensure_memory(stack);
    memcpy(at(stack->i, stack), elem, stack->elem_size);
    stack->i++;
}

void *spop(struct stack *stack) {
    ensure_memory(stack);
    stack->i--;
    return at(stack->i, stack);
}

size_t ssize(struct stack *stack) {
    return stack->i;
}

bool istop(void *elem, struct stack *stack) {
    return top(stack) == elem;
}

bool sempty(struct stack *stack) {
    return stack->i == 0;
}

void free_stack(struct stack *stack) {
    free(stack->buf);
    stack->buf = NULL;
    free(stack);
}
