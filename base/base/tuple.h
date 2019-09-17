#ifndef BASE_TUPLE_H_
#define BASE_TUPLE_H_ 1

struct tuple2 {
    void *fst;
    void *snd;
};

struct tuple2 tuple2(void *fst, void *snd);
void *fst(struct tuple2 *tuple);
void *snd(struct tuple2 *tuple);
void free_tuple2(struct tuple2 *tuple);

#endif // BASE_TUPLE_H_
