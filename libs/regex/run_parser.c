#include "regex/run_parser.h"
#include "regex/parser.h"
#include "regex/parser_nonrec.h"
#include "regex/parser_shared.h"

bool run_parser(struct parse_context *context) {
    if (context->use_nonrec) {
        return parse_regex_nonrec(context);
    } else {
        return parse_regex(context);
    }
}
