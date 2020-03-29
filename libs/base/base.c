#include <stdlib.h>
#include <stdarg.h>
#include "base/base.h"

void vfreel(void *p, va_list ptrs) {
    if (!p) return;
    do free(p);
    while ((p = va_arg(ptrs, void *)));
}

void freel(void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);
}

