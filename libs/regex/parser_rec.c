#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "regex/parser.h"
#include "regex/parser_rec.h"
#include "regex/result_types.h"

bool parse_regex(char *regex, struct parse_context *context) {
    start_scanning(regex, context);

    return parse_expr(context) &&
        expect(EOF_T, context) &&
        do_action(DO_REGEX, NULLRVAL, context);
}

bool parse_expr(struct parse_context *context) {
    if (peek(EOF_T, context) || peek(RPAREN_T, context))
        return do_action(DO_EMPTY, NULLRVAL, context);
    else if (parse_alt(context) && parse_alts(context))
        return true;

    return set_syntax_error(EXPR_NT, context);
}

bool parse_alt(struct parse_context *context) {
    bool success = do_action(DO_EMPTY, NULLRVAL, context);

    while (peek(FACTOR_NT, context) && success) {
        union regex_result prev_factor = result(context);

        success =
            parse_factor(context) &&
            do_action(DO_CAT, prev_factor, context);
    }

    return success || set_syntax_error(ALT_NT, context);
}

bool parse_alts(struct parse_context *context) {
    bool success = true;

    while (peek(ALT_T, context) && success) {
        union regex_result prev_alt = result(context);

        success =
            expect(ALT_T, context) &&
            parse_alt(context) &&
            do_action(DO_ALT, prev_alt, context);
    }

    return success || set_syntax_error(ALTS_NT, context);
}

bool parse_ranges(struct parse_context *context) {
    bool success = true;

    while (peek(RANGE_T, context) && success) {
        union regex_result range = lookahead_val(context);

        success =
            expect(RANGE_T, context) &&
            do_action(DO_RANGE, range, context);
    }

    return success || set_syntax_error(RANGES_NT, context);
}

bool parse_char_class(struct parse_context *context) {
    bool success = true;
    union regex_result head = lookahead_val(context);

    if (peek(RANGE_T, context) &&
        expect(RANGE_T, context) &&
        do_action(DO_RANGES, head, context)) {
        head = result(context);
        success = parse_ranges(context);
    } else {
        head = NULLRVAL;
    }

    success = success &&
        expect(END_CLASS_T, context) &&
        do_action(DO_CHAR_CLASS, head, context);

    return success || set_syntax_error(CHAR_CLASS_NT, context);
}

bool parse_factor(struct parse_context *context) {
    bool success = false;

    if (peek(LPAREN_T, context)) {
        success =
            expect(LPAREN_T, context) &&
            parse_expr(context) &&
            expect(RPAREN_T, context) &&
            do_action(DO_SUB, NULLRVAL, context);
    } else if (peek(ID_BRACE_T, context)) {
        success = expect(ID_BRACE_T, context);
        char idbuf[BUFSIZ] = "";
        union regex_result id = id_val(idbuf, context);
        success = success &&
            expect(ID_T, context) &&
            expect(RBRACE_T, context) &&
            do_action(DO_ID, id, context);
    } else if (peek(CLASS_T, context)) {
        success = expect(CLASS_T, context) && parse_char_class(context);
    } else if (peek(NEG_CLASS_T, context)) {
        success =
            expect(NEG_CLASS_T, context) &&
            parse_char_class(context) &&
            do_action(DO_NEG_CLASS, NULLRVAL, context);
    } else if (peek(DOTALL_T, context)) {
        success = expect(DOTALL_T, context) && do_action(DO_DOTALL, NULLRVAL, context);
    } else if (peek(SYMBOL_T, context)) {
        union regex_result sym = lookahead_val(context);
        success = expect(SYMBOL_T, context) && do_action(DO_SYMBOL, sym, context);
    }

    return success ? parse_unops(context) : set_syntax_error(FACTOR_NT, context);
}

bool parse_unops(struct parse_context *context) {
    bool success = true;

    while (success) {
        if (peek(STAR_T, context)) {
            success = expect(STAR_T, context) && do_action(DO_STAR, NULLRVAL, context);
        } else if (peek(PLUS_T, context)) {
            success = expect(PLUS_T, context) && do_action(DO_PLUS, NULLRVAL, context);
        } else if (peek(OPTIONAL_T, context)) {
            success = expect(OPTIONAL_T, context) && do_action(DO_OPTIONAL, NULLRVAL, context);
        } else if (peek(LBRACE_T, context)) {
            success = expect(LBRACE_T, context);
            union regex_result num = lookahead_val(context);
            success = success &&
                expect(NUM_T, context) &&
                expect(RBRACE_T, context) &&
                do_action(DO_REPEAT_EXACT, num, context);
        } else break;
    }

    return success || set_syntax_error(UNOPS_NT, context);
}
