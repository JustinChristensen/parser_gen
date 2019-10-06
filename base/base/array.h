#ifndef BASE_ARRAY_H_
#define BASE_ARRAY_H_ 1

#include <stdlib.h>
#include <stdbool.h>

#define GROWTH_FACTOR 1.6F
#define SHRINK_FACTOR 0.6F
#define SHRINK_SIZE 0.3F

struct array {
    void *buf;
    size_t i;
    size_t initsize;
    size_t size;
    size_t elem_size;
    float growth_factor;
};

struct array *init_array(size_t elem_size, size_t size, float growth_factor);
void *at(size_t i, struct array *array);
void *atop(struct array *array);
void *abottom(struct array *array);
void aresize(size_t size, struct array *array);
void areset(struct array *array);
void apush(void *elem, struct array *array);
void apop(void *out, struct array *array);
size_t asize(struct array *array);
bool aistop(void *elem, struct array *array);
bool aempty(struct array *array);
void free_array(struct array *array);

#endif // BASE_ARRAY_H_
