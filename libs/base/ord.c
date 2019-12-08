#include "base/ord.h"

int intcmp(int const *a, int const *b) {
    return *a - *b;
}

size_t max(size_t a, size_t b) {
    return a > b ? a : b;
}

size_t min(size_t a, size_t b) {
    return a < b ? a : b;
}
