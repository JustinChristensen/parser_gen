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
    bool frozen;
    float growth_factor;
};

struct array array(void *buf, size_t elem_size, size_t size, bool frozen, float growth_factor);
struct array *init_array(size_t elem_size, size_t size, bool frozen, float growth_factor);
void agfactor(float growth_factor, struct array *arr);
void afreeze(struct array *arr);
void asort(int (*compare)(void const *a, void const *b), struct array *arr);
bool arrayeq(
    bool (*eleq) (void const *a, void const *b),
    struct array const *a,
    struct array const *b
);
void *at(size_t i, struct array const *arr);
void *atop(struct array *arr);
void *abottom(struct array *arr);
void aresize(size_t size, struct array *arr);
void areset(struct array *arr);
void apush(void *elem, struct array *arr);
void apop(void *out, struct array *arr);
size_t asize(struct array const *arr);
bool aistop(void *elem, struct array *arr);
bool aempty(struct array const *arr);
void free_array(struct array *arr);

#endif // BASE_ARRAY_H_
