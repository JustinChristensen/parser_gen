// #include <stdlib.h>
// #include "base/array.h"
// #include "base/tuple.h"

// struct array array(void *arr, size_t size) {
//     void arr = calloc(sizeof *arr, size);
//     assert(arr != NULL);
//     struct array array = { arr, size };
//     return array;
// }
//
// struct tuple2 partition(void *arr, size_t size, bool (*predicate) (void *el)) {
//     struct tuple2 tuple = tuple2(NULL, NULL);
//
//     if (predicate) {
//         tuple.fst = array(size);
//         tuple.snd = array(size);
//         void *x = first(fst(tuple)),
//              *y = first(snd(tuple.snd));
//
//         for (int i = 0; i < size; i++) {
//             if ((*predicate)(arr[i])) {
//                 *x++ = arr[i];
//             } else {
//                 *y++ = arr[i];
//             }
//         }
//     }
//
//     return tuple;
// }
//
// void *first(struct array arr) {
//     return arr.arr;
// }
//
// size_t size(struct array arr) {
//     return arr.size;
// }
//
// void free_array(struct array *arr) {
//     free(arr->arr);
//     arr->arr = NULL;
//     arr->size = 0;
// }
