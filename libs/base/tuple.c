#include "base/tuple.h"

struct tuple2 tuple2(void *fst, void *snd) {
    return (struct tuple2) { fst, snd };
}

void *fst(struct tuple2 tuple) {
    return tuple.fst;
}

void *snd(struct tuple2 tuple) {
    return tuple.snd;
}

