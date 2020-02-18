#include "regex/run_parser.h"
#include "regex/parser.h"
#include "regex/parser_rec.h"
#include "regex/parser_nonrec.h"

bool run_parser(char *input, struct parse_context *context) {
    if (getenv("USE_NONREC")) {
        return parse_regex_nonrec(input, context);
    } else {
        return parse_regex(input, context);
    }
}
