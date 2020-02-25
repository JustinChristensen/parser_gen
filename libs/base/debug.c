#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "base/debug.h"
#include "base/string.h"

static void vdebug_ns_(char const *ns, char const *format, va_list args) {
    char *dns = getenv("DEBUG");
    if (dns && (streq("*", dns) || (streq(ns, dns)))) {
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
