#include <stdio.h>
#include "regex/base.h"

void regex_print_range(FILE *handle, struct char_range r) {
    fprintf(handle, "%c-%c", r.start, r.end);
}
