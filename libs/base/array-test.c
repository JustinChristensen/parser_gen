#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <base/array.h>
#include <base/macros.h>

struct coords {
    double lat;
    double lng;
};

#define TEST_SIZE 10000000

void print_coords(struct coords const *c) {
    printf("lat: %lf, lng: %lf\n", c->lat, c->lng);
}

int compare_coords(struct coords const *a, struct coords const *b) {
    return b->lat - a->lat;
}

bool coordseq(struct coords const *a, struct coords const *b) {
    printf("a: ");
    print_coords(a);
    printf("b: ");
    print_coords(b);
    return a->lat == b->lat && a->lng == b->lng;
}

int main(int argc, char *argv[]) {
    printf("sizeof(struct coords): %ld\n", sizeof(struct coords));
    printf("sizeof(struct array): %ld\n", sizeof(struct array));

    struct array *arr = init_array(sizeof(struct coords), 100, 0, 0);

    struct coords c;

    long i;

    for (i = 0; i < TEST_SIZE; i++) {
        c.lat = c.lng = (double) i;
        apush(&c, arr);
    }

    while (!aempty(arr)) {
        apop(&c, arr);

        if ((i - 1) % (TEST_SIZE / 10) == 0) {
            print_coords(&c);
        }

        i--;
    }

    struct coords
        c1 = { 11, 1 },
        c2 = { 1, 6 },
        c3 = { 5, 3 };

    afreeze(arr);

    apush(&c1, arr);
    apush(&c2, arr);
    apush(&c3, arr);

    asort(CMPFN compare_coords, arr);

    apop(&c1, arr);
    print_coords(&c1);
    apop(&c1, arr);
    print_coords(&c1);
    apop(&c1, arr);
    print_coords(&c1);

    free_array(arr);
    arr = NULL;

    struct array
        *a1 = init_array(sizeof(struct coords), 10, 0, 0),
        *a2 = init_array(sizeof(struct coords), 10, 0, 0);

    c1 = (struct coords) { 1, 2 };
    c2 = (struct coords) { 3, 4 };
    c3 = (struct coords) { 5, 6 };

    apush(&c1, a1);
    apush(&c1, a2);
    apush(&c2, a1);
    apush(&c2, a2);
    apush(&c3, a1);
    apush(&c3, a2);

    printf("a1 equals a2: %s\n", arrayeq(EQFN coordseq, a1, a2) ? "yes" : "no");

    apush(&c3, a2);
    printf("a1 equals a2: %s\n", arrayeq(EQFN coordseq, a1, a2) ? "yes" : "no");

    free_array(a1);
    free_array(a2);
    a1 = a2 = NULL;

    return 0;
}
