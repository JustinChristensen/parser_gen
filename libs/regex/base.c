#include <stdio.h>
#include "regex/base.h"

struct regex_loc regex_loc(int line, int col) {
    return (struct regex_loc) { line, col };
}

void regex_print_loc(FILE *handle, struct regex_loc loc) {
    fprintf(handle, "%d:%d", loc.line, loc.col);
}

void regex_print_range(FILE *handle, struct char_range r) {
    fprintf(handle, "%c-%c", r.start, r.end);
}

bool is_null_pattern(struct regex_pattern const *pattern) {
    return pattern->pattern == NULL;
}
