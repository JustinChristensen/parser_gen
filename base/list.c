#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include "base/list.h"

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

struct list *push(struct list *list, void *val) {
    struct node *node = malloc(sizeof *node);
    assert(node != NULL);

    node->val = val;
    node->next = NULL;

    if (list->head) {
        node->next = list->head;
        list->head = node;
    } else {
        list->head = list->last = node;
    }

    return list;
}

struct node *pop(struct list *list) {
    struct node *node = NULL;

    if (list->head) {
        node = list->head;
        list->head = node->next;
    }

    return node;
}

struct node *head(struct list *list) {
    return list->head;
}

struct node *next(struct node *node) {
    return node->next;
}

void *value(struct node *node) {
    return node->val;
}

bool empty(struct list *list) {
    return list->head == NULL;
}

void free_node(struct node *node, void (*free_val) (void *val)) {
    if (free_val) (*free_val)(node->val);
    free(node);
}

void free_list(struct list *list, void (*free_val) (void *val)) {
    struct node *node, *next = NULL;

    for (node = head(list); node; node = next) {
        next = node->next;
        free_node(node, free_val);
    }

    free(list);
}

