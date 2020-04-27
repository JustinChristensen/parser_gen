#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <base/assert.h>
#include <base/debug.h>
#include <regex/nfa.h>
#include "gram/spec.h"
#include "gram/analyze.h"
#include "gram/states.h"
#include "gram/slr.h"

#include "internal/assert.c"
#include "internal/macros.c"

#define debug(...) debug_ns("gram_slr", __VA_ARGS__);

// struct slr_parser slr_parser(
//     struct nfa_context scanner, struct gram_stats stats
// ) {
// }

bool gen_slr(
    struct slr_error *error, struct slr_parser *parser,
    struct gram_parser_spec *spec
) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);

    return true;
}

// void print_slr_parser(FILE *handle, struct slr_parser *parser);
// void fre_slr_parser(struct slr_parser *parser);
//
// void print_slr_error(FILE *handle, struct slr_error error);
