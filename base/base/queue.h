#ifndef BASE_QUEUE_H_
#define BASE_QUEUE_H_ 1

#include <stdbool.h>
#include "list.h"

typedef struct list queue;

queue *init_queue();
queue *enq(queue *queue, void *val);
struct node *dq(queue *queue);
struct node *peek(queue *queue);
bool empty(queue *queue);
void free_queue(queue *queue, void (*free_val) (void *val));

#endif // BASE_QUEUE_H_

