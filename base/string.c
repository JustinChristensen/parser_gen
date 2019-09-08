#include "base/string.h"

struct sizenl numlines(char *str) {
    struct sizenl stats = { 0, 0 };

    while (*str != '\0') {
        stats.sl++;
        if (*str == '\n') stats.nl++;
        str++;
    }

    return stats;
}
