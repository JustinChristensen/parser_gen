#ifndef BASE_STACK_H_
#define BASE_STACK_H_ 1

#include <stdlib.h>
#include <stdbool.h>

#define GROWTH_FACTOR 1.6F
#define SHRINK_FACTOR 0.6F
#define SHRINK_SIZE 0.3F

struct stack {
    void *buf;
    size_t i;
    size_t isize;
    size_t size;
    size_t elem_size;
    float growth_factor;
};

struct stack *init_stack(size_t elem_size, size_t size, float growth_factor);
void *at(size_t i, struct stack *stack);
void *top(struct stack *stack);
void *bottom(struct stack *stack);
void resize(size_t size, struct stack *stack);
void reset(struct stack *stack);
void spush(void *elem, struct stack *stack);
void *spop(struct stack *stack);
size_t ssize(struct stack *stack);
bool istop(void *elem, struct stack *stack);
bool sempty(struct stack *stack);
void free_stack(struct stack *stack);

#endif // BASE_STACK_H_

