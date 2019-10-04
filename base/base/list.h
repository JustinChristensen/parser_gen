#ifndef BASE_LIST_H_
#define BASE_LIST_H_ 1

#include <stdbool.h>

typedef struct list list;

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
struct node *next(struct node *node);
struct list *push(struct list *list, void *val);
struct node *pop(struct list *list);
void *value(struct node *node);
bool empty(struct list *list);
void free_node(struct node *node, void (*free_val) (void *val));
void free_list(struct list *list, void (*free_val) (void *val));

#endif // BASE_LIST_H_
