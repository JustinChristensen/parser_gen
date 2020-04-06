#ifndef GRAM_PACK_H_
#define GRAM_PACK_H_ 1

#include "gram/parser.h"
#include "gram/spec.h"

bool gram_pack(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    struct gram_parse_context *context
);

#endif // GRAM_PACK_H_

