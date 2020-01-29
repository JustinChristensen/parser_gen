#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "regex/parser.h"
#include "regex/parser_rec.h"
#include "regex/result_types.h"

bool parse_regex(char *regex, struct parse_context *context) {
    start_scanning(regex, context);

    if (parse_alts(context) && expect(EOF_T, context)) {
        do_action(DO_REGEX, NULLRVAL, context);
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
    bool success = true;

    while (true) {
        if (peek(END_CLASS_T, context)) break;
        else {
            union regex_result range = lookahead_val(context);

            if (expect(RANGE_T, context)) {
                do_action(DO_RANGE, range, context);
                continue;
            } else success = false;
        }

        break;
    }

    if (success) return true;

    set_parse_error(RANGES_NT, context);

    return false;
}

bool parse_factor(struct parse_context *context) {
    bool success = false;

    if (peek(LPAREN_T, context)) {
        success = expect(LPAREN_T, context) &&
                  parse_alts(context) &&
                  expect(RPAREN_T, context);
        if (success) do_action(DO_SUB, NULLRVAL, context);
    } else if (peek(LBRACE_T, context)) {
        success = expect(LBRACE_T, context);
        char idbuf[BUFSIZ] = "";
        union regex_result id = id_val(idbuf, context);
        success = success && expect(ID_T, context) && expect(RBRACE_T, context);
        if (success) do_action(DO_ID, id, context);
    } else if (peek(CLASS_T, context)) {
        success = expect(CLASS_T, context) &&
                  parse_ranges(context) &&
                  expect(END_CLASS_T, context);
        if (success) do_action(DO_CHAR_CLASS, NULLRVAL, context);
    } else if (peek(NEG_CLASS_T, context)) {
        success = expect(NEG_CLASS_T, context) &&
                  parse_ranges(context) &&
                  expect(END_CLASS_T, context);
        if (success) do_action(DO_NEG_CLASS, NULLRVAL, context);
    } else if (peek(DOTALL_T, context)) {
        success = expect(DOTALL_T, context);
        do_action(DO_DOTALL, NULLRVAL, context);
    } else if (peek(SYMBOL_T, context)) {
        union regex_result sym = lookahead_val(context);
        success = expect(SYMBOL_T, context);
        do_action(DO_SYMBOL, sym, context);
    }

    if (success && parse_unops(context))
        return true;

    set_parse_error(FACTOR_NT, context);

    return false;
}

bool parse_unops(struct parse_context *context) {
    bool success = true;

    while (true) {
        if (peek(STAR_T, context) && expect(STAR_T, context)) {
            do_action(DO_STAR, NULLRVAL, context);
            continue;
        } else if (peek(PLUS_T, context) && expect(PLUS_T, context)) {
            do_action(DO_PLUS, NULLRVAL, context);
            continue;
        } else if (peek(OPTIONAL_T, context) && expect(OPTIONAL_T, context)) {
            do_action(DO_OPTIONAL, NULLRVAL, context);
            continue;
        } else if (peek(LBRACE_T, context)) {
            success = expect(LBRACE_T, context);
            union regex_result num = lookahead_val(context);
            success = success && expect(NUM_T, context) && expect(RBRACE_T, context);

            if (success) {
                do_action(DO_REPEAT_EXACT, num, context);
                continue;
            }
        }

        break;
    }

    return success;
}
