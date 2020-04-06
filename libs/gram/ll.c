#include "gram/ll.h"

static bool parse_table(struct gram_ll_error *error, struct gram_ll_parser *parser, struct gram_parser_spec *spec) {
}

static bool rule_table(struct gram_ll_error *error, struct gram_ll_parser *parser, struct gram_parser_spec *spec) {
}

bool genll(struct gram_ll_error *error, struct gram_ll_parser *parser, struct gram_parser_spec *spec) {
    return parse_table(error, parser, spec) &&
        rule_table(error, parser, spec);
}

