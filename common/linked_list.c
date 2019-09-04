#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include "linked_list.h"

struct node_list *init_list() {
    struct node_list *list = malloc(*list);
    assert(list != NULL);
    list->head = list->last = NULL;
    return list;
}

struct node_list *append(struct node_list *list, void *val) {
    struct node *node = malloc(*node);
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

struct node *head(struct node_list *list) {
    return list->head;
}

void *value(struct node *node) {
    return node->val;
}

bool empty(struct node_list *list) {
    return list->last == NULL;
}

void free_list(struct node_list *list, void (*free_val) (void *val)) {
    struct node *node, *next = NULL;

    for (node = head(list); node; node = next) {
        (*free_val)(node->val);
        next = node->next;
        free(node);
    }

    free(list);
}

