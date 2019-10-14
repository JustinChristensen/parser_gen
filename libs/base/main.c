#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <base/array.h>

struct coords {
    double lat;
    double lng;
};

#define TEST_SIZE 10000000

int main(int argc, char *argv[]) {
    printf("sizeof(struct coords): %ld\n", sizeof(struct coords));
    printf("sizeof(struct array): %ld\n", sizeof(struct array));

    struct array *arr = init_array(sizeof(struct coords), 100, 0);

    struct coords c;

    long i = 0;

    for (; i < TEST_SIZE; i++) {
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

    free_array(arr);

    return 0;
}
