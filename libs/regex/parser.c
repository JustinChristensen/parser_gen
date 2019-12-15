#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "regex/parser.h"
#include "regex/parser_shared.h"
#include "regex/result_types.h"

bool parse_regex(struct parse_context *context) {
    if (parse_expr(context) && expect(context, '\0', NULL)) {
        do_action(context, DO_REGEX, NULLRVAL);
        return true;
    }

    return false;
}

bool parse_expr(struct parse_context *context) {
    parse_alt(context, getval(context));
    return true;
}

bool parse_alt(struct parse_context *context, union rval lval) {
    if (parse_cat(context, lval)) {
        while (true) {
            lval = getval(context);

            if (peek(context, ALT, NULL) &&
                expect(context, ALT, NULL) &&
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

bool parse_cat(struct parse_context *context, union rval lval) {
    do_action(context, DO_EMPTY, NULLRVAL);
    union rval empty = getval(context);

    if (parse_factor(context)) {
        while (true) {
            lval = getval(context);

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

    if (peek(context, LPAREN, NULL) &&
        expect(context, LPAREN, NULL) &&
        parse_expr(context) && expect(context, RPAREN, NULL)) {
        do_action(context, DO_SUB, NULLRVAL);
        has_head = true;
    } else if (peek(context, DOTALL, NULL)) {
        expect(context, DOTALL, NULL);
        do_action(context, DO_DOTALL, NULLRVAL);
        has_head = true;
    } else if (peek(context, SYMBOL, is_symbol)) {
        union rval sym = { .sym = lookahead(context) };
        expect(context, SYMBOL, is_symbol);
        do_action(context, DO_SYMBOL, sym);
        has_head = true;
    }

    if (has_head) {
        while (true) {
            if (peek(context, STAR, NULL) && expect(context, STAR, NULL)) {
                do_action(context, DO_STAR, NULLRVAL);
                continue;
            } else if (peek(context, PLUS, NULL) && expect(context, PLUS, NULL)) {
                do_action(context, DO_PLUS, NULLRVAL);
                continue;
            } else if (peek(context, OPTIONAL, NULL) && expect(context, OPTIONAL, NULL)) {
                do_action(context, DO_OPTIONAL, NULLRVAL);
                continue;
            }

            break;
        }

        return true;
    }

    return false;
}

