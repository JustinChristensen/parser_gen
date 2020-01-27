#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "regex/parser.h"
#include "regex/parser_rec.h"
#include "regex/result_types.h"

bool parse_regex(char *regex, struct parse_context *context) {
    start_scanning(regex, context);

    if (parse_expr(context) && expect(context, EOF_T)) {
        do_action(context, DO_REGEX, NULLRVAL);
        return true;
    }

    return false;
}

bool parse_expr(struct parse_context *context) {
    parse_alt(context);
    return true;
}

bool parse_alt(struct parse_context *context) {
    if (parse_cat(context)) {
        while (true) {
            union rval lval = getval(context);

            // FIXME: should this really be expr?
            if (peek(context, ALT_T) &&
                expect(context, ALT_T) &&
                parse_expr(context)) {
                do_action(context, DO_ALT, lval);
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

bool parse_cat(struct parse_context *context) {
    do_action(context, DO_EMPTY, NULLRVAL);
    union rval empty = getval(context);

    if (parse_factor(context)) {
        while (true) {
            union rval lval = getval(context);

            if (parse_factor(context)) {
                do_action(context, DO_CAT, lval);
                continue;
            }

            break;
        }

        do_action(context, DO_CAT, empty);

        return true;
    }

    return false;
}

bool parse_factor(struct parse_context *context) {
    bool has_head = false;

    // FIXME: this doesn't distinguish between selecting ε for expr and
    // a parse error encountered during expr
    if (peek(context, LPAREN_T) &&
        expect(context, LPAREN_T) &&
        parse_expr(context) && expect(context, RPAREN_T)) {
        do_action(context, DO_SUB, NULLRVAL);
        has_head = true;
    } else if (peek(context, DOTALL_T)) {
        expect(context, DOTALL_T);
        do_action(context, DO_DOTALL, NULLRVAL);
        has_head = true;
    } else if (peek(context, SYMBOL_T)) {
        union rval sym = { .sym = symbol(context) };
        expect(context, SYMBOL_T);
        do_action(context, DO_SYMBOL, sym);
        has_head = true;
    }

    if (has_head) {
        while (true) {
            if (peek(context, STAR_T) && expect(context, STAR_T)) {
                do_action(context, DO_STAR, NULLRVAL);
                continue;
            } else if (peek(context, PLUS_T) && expect(context, PLUS_T)) {
                do_action(context, DO_PLUS, NULLRVAL);
                continue;
            } else if (peek(context, OPTIONAL_T) && expect(context, OPTIONAL_T)) {
                do_action(context, DO_OPTIONAL, NULLRVAL);
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

