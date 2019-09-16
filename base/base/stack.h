#ifndef BASE_STACK_H_
#define BASE_STACK_H_ 1

#include <stdbool.h>
#include "list.h"

typedef struct list stack;

stack *init_stack();
stack *push(stack *stack, void *val);
struct node *pop(stack *stack);
struct node *peek(stack *stack);
bool empty(stack *stack);
void free_stack(stack *stack, void (*free_val) (void *val));

#endif // BASE_STACK_H_

