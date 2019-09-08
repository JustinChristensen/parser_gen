#ifndef BASE_LINKED_LIST_H_
#define BASE_LINKED_LIST_H_ 1

#include <stdbool.h>

// actually a queue that doesn't support dequeue
struct list {
    struct node *head;
    struct node *last;
};

struct node {
    struct node *next;
    void *val;
};

struct list *init_list();
struct list *append(struct list *list, void *val);
struct node *head(struct list *list);
void *value(struct node *node);
bool empty(struct list *list);
void free_list(struct list *list, void (*free_val) (void *val));

#endif // BASE_LINKED_LIST_H_
