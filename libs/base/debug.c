#include <stdlib.h>
#include <stdio.h>
#include "base/debug.h"

static void vdebug_ns_(char const *ns, char const *format, va_list args) {
    if (getenv("DEBUG")) {
        if (ns) fprintf(stderr, "[%s]: ", ns);
        vfprintf(stderr, format, args);
        va_end(args);
    }
}

void debug_ns_(char const *ns, char const *format, ...) {
    va_list args;
    va_start(args, format);
    vdebug_ns_(ns, format, args);
    va_end(args);
}

void debug_(char const *format, ...) {
    va_list args;
    va_start(args, format);
    vdebug_ns_(NULL, format, args);
    va_end(args);
}
