#include <stdio.h>
#include "base/string.h"

struct sizenl numlines(char *str) {
    struct sizenl stats = { 0, 0 };

    while (str && *str != '\0') {
        stats.sl++;
        if (*str == '\n') stats.nl++;
        str++;
    }

    return stats;
}

char *repeat(char *str, char c, int times) {
    while (times-- > 0) *str++ = c;
    *str = '\0';
    return str;
}

char *put(char *str, char *s) {
    while (*s != '\0') {
        *str++ = *s++;
    }
    *str = '\0';
    return str;
}

char *putln(char *str, char *s) {
    str = put(str, s);
    *str++ = '\n';
    *str = '\0';
    return str;
}

void indent(int n) {
    while (n-- > 0) printf("    ");
}

void printstr(char const *s) {
    printf("%s", s);
}
