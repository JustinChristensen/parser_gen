#include "base/tuple.h"

struct tuple2 tuple2(void *fst, void *snd) {
    struct tuple2 tuple = { fst, snd };
    return tuple;
}

void *fst(struct tuple2 tuple) {
    return tuple.fst;
}

void *snd(struct tuple2 tuple) {
    return tuple.snd;
}

void free_tuple2(struct tuple2 *tuple) {
    free(tuple->fst);
    free(tuple->snd);
    tuple->fst = tuple->snd = NULL;
}

