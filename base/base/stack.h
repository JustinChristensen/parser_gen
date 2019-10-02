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
    size_t size;
    size_t elem_size;
    float growth_factor;
};

struct stack *init_stack(size_t elem_size, size_t size, float growth_factor);
void *top(struct stack *stack);
void *bottom(struct stack *stack);
void resize(size_t size, struct stack *stack);
void push(void *elem, struct stack *stack);
void *pop(struct stack *stack);
void clear(struct stack *stack);
bool sempty(struct stack *stack);
void free_stack(struct stack *stack);

#endif // BASE_STACK_H_

