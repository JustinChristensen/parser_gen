#ifndef BASE_ARRAY_H_
#define BASE_ARRAY_H_ 1

#include <stdlib.h>
#include <stdbool.h>

#define GROWTH_FACTOR 1.6F
#define SHRINK_FACTOR 0.6F
#define GROWTH_CONSTANT 1
#define SHRINK_SIZE 0.3F

enum growth {
    EXPONENTIAL,
    LINEAR,
    FROZEN
};

struct array {
    void *buf;                 // the buffer
    enum growth growth;        // 1, 2, 4, ... vs 1, 2, 3, ...
    float factor;              // (* 2) or (+ 1)
    int i;                     // current position
    int init_size;             // initial size (will not shrink below this)
    int size;                  // current allocated size
    int elem_size;             // size of an element
};

struct array array(void *buf, size_t elem_size, int init_size, enum growth growth, float factor);
struct array *init_array(size_t elem_size, int init_size, enum growth growth, float factor);
void agfactor(enum growth growth, float factor, struct array *arr);
void afreeze(struct array *arr);
void asort(int (*compare)(void const *a, void const *b), struct array *arr);
bool arrayeq(
    bool (*eleq) (void const *a, void const *b),
    struct array const *a,
    struct array const *b
);
void aresize(int size, struct array *arr);
void adel(void *elem, struct array *arr);
void *aptr(int i, struct array const *arr);
void at(void *out, int i, struct array const *arr);
void apush(void *elem, struct array *arr);
void apop(void *out, struct array *arr);
void apeek(void *out, struct array const *arr);
void abottom(void *out, struct array const *arr);
void areset(struct array *arr);
int asize(struct array const *arr);
bool aempty(struct array const *arr);
void free_array(struct array *arr);

#endif // BASE_ARRAY_H_
