#ifndef GRAM_LR_C_
#define GRAM_LR_C_ 1

#include <stdbool.h>
#include <stdarg.h>
#include <base/base.h>
#include "gram/lr.h"

#define ACCEPT() (struct lr_action) { GM_LR_ACCEPT }
#define SHIFT(num) (struct lr_action) { GM_LR_SHIFT, (num) }
#define REDUCE(num, nt) (struct lr_action) { GM_LR_REDUCE, (num), (nt) }
#define GOTO(num) (struct lr_action) { GM_LR_GOTO, (num) }

static bool _oom_error(struct lr_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct lr_error) { .type = GM_LR_OOM_ERROR, .file = file, .col = col }));

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

#endif // GRAM_LR_C_
