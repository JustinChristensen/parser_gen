#ifndef REGEX_PARSER_REC_C_
#define REGEX_PARSER_REC_C_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "regex/base.h"

static bool parse_expr(struct regex_parse_context *context);
static bool parse_alt(struct regex_parse_context *context);
static bool parse_alts(struct regex_parse_context *context);
static bool parse_ranges(struct regex_parse_context *context);
static bool parse_char_class(struct regex_parse_context *context);
static bool parse_factor(struct regex_parse_context *context);
static bool parse_unops(struct regex_parse_context *context);

static bool parse_regex_rec(char *pattern, struct regex_loc loc, struct regex_parse_context *context) {
    start_scanning(pattern, loc, context);

    return (parse_expr(context) &&
        expect(RX_EOF_T, context) &&
        do_action(RX_DO_REGEX, RX_NULL_RESULT, context)) ||
        set_syntax_error(RX_REGEX_NT, context);
}

static bool parse_expr(struct regex_parse_context *context) {
    if (peek(RX_EOF_T, context) || peek(RX_RPAREN_T, context))
        return do_action(RX_DO_EMPTY, RX_NULL_RESULT, context);
    else if (parse_alt(context) && parse_alts(context))
        return true;

    return set_syntax_error(RX_EXPR_NT, context);
}

static bool parse_alt(struct regex_parse_context *context) {
    bool success = do_action(RX_DO_EMPTY, RX_NULL_RESULT, context);

    while (peek(RX_FACTOR_NT, context) && success) {
        union regex_result prev_factor = get_result(context);

        success =
            parse_factor(context) &&
            do_action(RX_DO_CAT, prev_factor, context);
    }

    return success || set_syntax_error(RX_ALT_NT, context);
}

static bool parse_alts(struct regex_parse_context *context) {
    bool success = true;

    while (peek(RX_ALT_T, context) && success) {
        union regex_result prev_alt = get_result(context);

        success =
            expect(RX_ALT_T, context) &&
            parse_alt(context) &&
            do_action(RX_DO_ALT, prev_alt, context);
    }

    return success || set_syntax_error(RX_ALTS_NT, context);
}

static bool parse_ranges(struct regex_parse_context *context) {
    bool success = true;

    while (peek(RX_RANGE_T, context) && success) {
        union regex_result range = lookahead_val(context);

        success =
            expect(RX_RANGE_T, context) &&
            do_action(RX_DO_RANGE, range, context);
    }

    return success || set_syntax_error(RX_RANGES_NT, context);
}

static bool parse_char_class(struct regex_parse_context *context) {
    bool success = true;
    union regex_result head = lookahead_val(context);

    if (peek(RX_RANGE_T, context) &&
        expect(RX_RANGE_T, context) &&
        do_action(RX_DO_RANGES, head, context)) {
        head = get_result(context);
        success = parse_ranges(context);
    } else {
        head = RX_NULL_RESULT;
    }

    success = success &&
        expect(RX_END_CLASS_T, context) &&
        do_action(RX_DO_CHAR_CLASS, head, context);

    return success || set_syntax_error(RX_CHAR_CLASS_NT, context);
}

static bool parse_factor(struct regex_parse_context *context) {
    bool success = false;

    if (peek(RX_LPAREN_T, context)) {
        success =
            expect(RX_LPAREN_T, context) &&
            parse_expr(context) &&
            expect(RX_RPAREN_T, context) &&
            do_action(RX_DO_SUB, RX_NULL_RESULT, context);
    } else if (peek(RX_TAG_BRACE_T, context)) {
        success = expect(RX_TAG_BRACE_T, context);
        char tagbuf[BUFSIZ] = "";
        union regex_result tag = tag_val(tagbuf, context);
        success = success &&
            expect(RX_TAG_T, context) &&
            expect(RX_RBRACE_T, context) &&
            do_action(RX_DO_TAG, tag, context);
    } else if (peek(RX_CLASS_T, context)) {
        success = expect(RX_CLASS_T, context) && parse_char_class(context);
    } else if (peek(RX_NEG_CLASS_T, context)) {
        success =
            expect(RX_NEG_CLASS_T, context) &&
            parse_char_class(context) &&
            do_action(RX_DO_NEG_CLASS, RX_NULL_RESULT, context);
    } else if (peek(RX_DOTALL_T, context)) {
        success = expect(RX_DOTALL_T, context) && do_action(RX_DO_DOTALL, RX_NULL_RESULT, context);
    } else if (peek(RX_CHAR_T, context)) {
        union regex_result ch = lookahead_val(context);
        success = expect(RX_CHAR_T, context) && do_action(RX_DO_CHAR, ch, context);
    }

    return success ? parse_unops(context) : set_syntax_error(RX_FACTOR_NT, context);
}

static bool parse_unops(struct regex_parse_context *context) {
    bool success = true;

    while (success) {
        if (peek(RX_STAR_T, context)) {
            success = expect(RX_STAR_T, context) && do_action(RX_DO_STAR, RX_NULL_RESULT, context);
        } else if (peek(RX_PLUS_T, context)) {
            success = expect(RX_PLUS_T, context) && do_action(RX_DO_PLUS, RX_NULL_RESULT, context);
        } else if (peek(RX_OPTIONAL_T, context)) {
            success = expect(RX_OPTIONAL_T, context) && do_action(RX_DO_OPTIONAL, RX_NULL_RESULT, context);
        } else if (peek(RX_LBRACE_T, context)) {
            success = expect(RX_LBRACE_T, context);
            union regex_result num = lookahead_val(context);
            success = success &&
                expect(RX_NUM_T, context) &&
                expect(RX_RBRACE_T, context) &&
                do_action(RX_DO_REPEAT_EXACT, num, context);
        } else break;
    }

    return success || set_syntax_error(RX_UNOPS_NT, context);
}

#endif // REGEX_PARSER_REC_C_
