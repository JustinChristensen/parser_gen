#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include "base/linked_list.h"

struct list *init_list() {
    struct list *list = malloc(sizeof *list);
    assert(list != NULL);
    list->head = list->last = NULL;
    return list;
}

struct list *append(struct list *list, void *val) {
    struct node *node = malloc(sizeof *node);
    assert(node != NULL);

    node->val = val;
    node->next = NULL;

    if (list->last) {
        list->last->next = node;
        list->last = node;
    } else {
        list->head = list->last = node;
    }

    return list;
}

struct node *head(struct list *list) {
    return list->head;
}

void *value(struct node *node) {
    return node->val;
}

bool empty(struct list *list) {
    return list->last == NULL;
}

void free_list(struct list *list, void (*free_val) (void *val)) {
    struct node *node, *next = NULL;

    for (node = head(list); node; node = next) {
        (*free_val)(node->val);
        next = node->next;
        free(node);
    }

    free(list);
}

