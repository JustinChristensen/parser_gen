#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <base/array.h>
#include <base/macros.h>

struct coords {
    double lat;
    double lng;
};

#define TEST_SIZE 10000000

int compare_coords(struct coords const *a, struct coords const *b) {
    return b->lat - a->lat;
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

        if (i % (TEST_SIZE / 10) == 0) {
            printf("lat: %lf, lng: %lf\n", c.lat, c.lng);
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
    printf("lat: %lf, lng: %lf\n", c1.lat, c1.lng);
    apop(&c1, arr);
    printf("lat: %lf, lng: %lf\n", c1.lat, c1.lng);
    apop(&c1, arr);
    printf("lat: %lf, lng: %lf\n", c1.lat, c1.lng);

    free_array(arr);

    return 0;
}
