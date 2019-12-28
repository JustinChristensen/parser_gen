#include <stdlib.h>
#include <stdio.h>
#include "base/debug.h"

void debug_ns_(char const *ns, char const *format, ...) {
    if (getenv("DEBUG")) {
        va_list args;
        va_start(args, format);
        if (ns) fprintf(stderr, "[%s]: ", ns);
        fprintf(stderr, format, args);
        va_end(args);
    }
}

void debug_(char const *format, ...) {
    va_list args;
    va_start(args, format);
    debug_ns_(NULL, format, args);
    va_end(args);
}
