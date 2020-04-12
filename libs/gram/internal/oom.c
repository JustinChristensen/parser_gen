#ifndef GRAM_OOM_C_
#define GRAM_OOM_C_ 1

#include <stdbool.h>
#include <stdarg.h>
#include <base/base.h>
#include "gram/parser.h"

static bool
_oom_error(struct gram_parse_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct gram_parse_error) { .type = GM_PARSER_OOM_ERROR, .file = file, .col = col }));

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

#endif // GRAM_OOM_C_
