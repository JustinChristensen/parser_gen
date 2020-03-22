#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "base/debug.h"
#include "base/string.h"

bool debug_is(char const *ns) {
    char *dns = getenv("DEBUG");
    return dns && (streq("*", dns) || (streq(ns, dns)));
}

static void vdebug_ns_(char const *ns, char const *format, va_list args) {
    if (debug_is(ns)) {
        vfprintf(stderr, format, args);
        va_end(args);
    }
}

void debug_ns(char const *ns, char const *format, ...) {
    va_list args;
    va_start(args, format);
    vdebug_ns_(ns, format, args);
    va_end(args);
}
