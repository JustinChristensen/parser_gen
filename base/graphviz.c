#include <stdlib.h>
#include <math.h>
#include "base/graphviz.h"
#include "base/string.h"

char *left_justify(char *str) {
    struct sizenl sz = numlines(str);
    char *newstr = calloc(sizeof *newstr, sz.sl + sz.nl + 1);

    char *c = str,
         *d = newstr;
    while (*c != '\0') {
        if (*c == '\n') {
            *d++ = '\\';
            *d++ = 'l';
        } else {
            *d++ = *c;
        }
        c++;
    }

    return newstr;
}
