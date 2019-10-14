#include "base/random.h"
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

static int seeded = false;

int randr(int min, int max) {
    if (!seeded) {
        srand(time(0));
        seeded = true;
    }

    return rand() % (max + 1 - min) + min;
}
