#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "regex/parser.h"
#include "regex/parser_rec.h"
#include "regex/result_types.h"

bool parse_regex(char *regex, struct parse_context *context) {
    start_scanning(regex, context);

    if (parse_alts(context) && expect(EOF_T, context)) {
        do_action(context, DO_REGEX, NULLRVAL);
        return true;
    }

    return false;
}

bool parse_alts(struct parse_context *context) {
    if (peek(RPAREN_T, context) || peek(EOF_T, context)) {
        return true;
    } else if (parse_alt(context)) {
        bool success = true;

        while (true) {
            union regex_result prev_alt = result(context);

            if (peek(ALT_T, context)) {
                success = expect(ALT_T, context) && parse_alt(context);

                if (success) {
                    do_action(DO_ALT, prev_alt, context);
                    continue;
                }
            }

            break;
        }

        return success;
    }

    set_parse_error(ALTS_NT, context);

    return false;
}

bool parse_alt(struct parse_context *context) {
    bool success = true;

    do_action(DO_EMPTY, NULLRVAL, context);

    while (true) {
        union regex_result prev_factor = result(context);

        if (peek(ALT_T, context) ||
            peek(RPAREN_T, context) ||
            peek(EOF_T, context)) break;

        if ((success = parse_factor(context))) {
            do_action(DO_CAT, prev_factor, context);
            continue;
        }

        break;
    }

    if (success) return true;

    set_parse_error(ALT_NT, context);

    return false;
}

bool parse_ranges(struct parse_context *context) {
}

bool parse_factor(struct parse_context *context) {
    bool success = false;

    if (peek(LPAREN_T, context)) {
        success = expect(LPAREN_T, context) &&
                  parse_alts(context) &&
                  expect(RPAREN_T, context);
        if (success) do_action(context, DO_SUB, NULLRVAL);
    } else if (peek(LBRACE_T, context)) {
    } else if (peek(CLASS_T, context)) {
    } else if (peek(NEG_CLASS_T, context)) {
    } else if (peek(DOTALL_T, context)) {
        success = expect(DOTALL_T, context);
        do_action(DO_DOTALL, NULLRVAL, context);
    } else if (peek(SYMBOL_T, context)) {
        union regex_result sym = { .sym = symbol(context) };
        success = expect(SYMBOL_T, context);
        do_action(DO_SYMBOL, sym, context);
    }

    if (success && parse_unops(context))
        return true;

    set_parse_error(FACTOR_NT, context);

    return false;
}

bool parse_unops(struct parse_context *context) {
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
