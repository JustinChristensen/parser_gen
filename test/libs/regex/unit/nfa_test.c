#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <regex/nfa.h>
#include "suites.h"

struct nfa_context context;

static void setup() {
}

static void teardown() {
    free_nfa_context(&context);
}

static void print_if_error() {
    if (nfa_has_error(&context)) {
        print_regex_error(stderr, nfa_error(&context));
    }
}

START_TEST(test_init_with_empty_patterns) {
    bool success = nfa_context(&context, RX_PATTERNS {
        RX_END_PATTERNS
    });

    print_if_error();

    ck_assert_msg(success, "init with empty patterns failed");
}
END_TEST

START_TEST(test_init_with_invalid_patterns) {
    ck_assert_msg(nfa_context(&context, RX_PATTERNS {
        { 0, NULL, "{5}" },
        RX_END_PATTERNS
    }) == false, "init with invalid patterns succeeded");

    ck_assert(nfa_error(&context).type == RX_SYNTAX_ERROR);
}
END_TEST

START_TEST(test_init_with_duplicate_tags) {
    ck_assert_msg(nfa_context(&context, RX_PATTERNS {
        { 0, "abc", "" },
        { 1, "abc", "" },
        RX_END_PATTERNS
    }) == false, "init with duplicate tags succeeded");

    ck_assert(nfa_error(&context).type == RX_TAG_EXISTS);
}
END_TEST

START_TEST(test_init_with_patterns) {
    bool success = nfa_context(&context, RX_PATTERNS {
        RX_ALNUM_(RX_TAG_ONLY),
        { 0, "abc", "a|b|c" },
        { 1, "a!", "a[?!]" },
        { 2, NULL, "{alnum_}{5}" },
        { 3, "e's", "([0-7]|u)|e+" },
        { 4, "aa",  ".." },
        { 5, "n followed by optional binary operator",  "[0-9]+[\t ]*(\\+|-|\\*|/)" },
        RX_END_PATTERNS
    });

    print_if_error();

    ck_assert_msg(success, "init with valid patterns failed");
}
END_TEST

START_TEST(test_scanner_is_reentrant) {
    ck_assert(nfa_context(&context, RX_PATTERNS {
        { 0, NULL, "var" },
        { 1, NULL, "[a-z]+" },
        { 2, NULL, "[0-9]+" },
        { 3, NULL, "=" },
        { 4, NULL, ";" },
        { 5, NULL, " *" },
        RX_END_PATTERNS
    }));

    char *input = "var foo = 33;";
    int const ENDT = -10;
    int tokens[] = { 0, 5, 1, 5, 3, 5, 2, 4, RX_EOF, ENDT };

    struct nfa_match match = {0};
    ck_assert(nfa_start_match(input, &match, &context));

    for (int i = 0; i < 3; i++) {
        int expected = 0;
        for (int j = 0; (expected = tokens[j]) != ENDT; j++) {
            ck_assert_int_eq(nfa_match(&match), expected);
        }
    }

    free_nfa_match(&match);
}
END_TEST

enum {
    _RESERVED = 1,
    _IDENT = 2,
    _NUM = 3,
    _ADD = 4,
    _SUB = 5,
    _MULT = 6,
    _DIV = 7,
    _LPAREN = 8,
    _RPAREN = 9,
    _WHITESPACE = 10
};

START_TEST(test_scans_simple_expression) {
    ck_assert(nfa_context(&context, NULL));

    bool success =
        nfa_regex(RX_TAG_ONLY, "alpha_", "[a-zA-Z_]", &context) &&
        nfa_regex(RX_TAG_ONLY, "alnum_", "[0-9a-zA-Z_]", &context) &&
        nfa_regex(_RESERVED, NULL, "RESERVED", &context) &&
        nfa_regex(_IDENT, NULL, "{alpha_}{alnum_}*", &context) &&
        nfa_regex(_NUM, NULL, "[0-9]+", &context) &&
        nfa_regex(_ADD, NULL, "\\+", &context) &&
        nfa_regex(_SUB, NULL, "-", &context) &&
        nfa_regex(_MULT, NULL, "\\*", &context) &&
        nfa_regex(_DIV, NULL, "/", &context) &&
        nfa_regex(_LPAREN, NULL, "\\(", &context) &&
        nfa_regex(_RPAREN, NULL, "\\)", &context) &&
        nfa_regex(_WHITESPACE, NULL, "[\t ]*", &context);

    ck_assert_msg(success, "init expression lexer failed");

    char *expr = "(foo + 33)   * 3691 \t - (bar_ - BAZ/RESERVED)";

    int const ENDT = -10;
    int tokens[] = {
        _LPAREN, _IDENT, _WHITESPACE, _ADD, _WHITESPACE, _NUM, _RPAREN,
        _WHITESPACE, _MULT, _WHITESPACE, _NUM, _WHITESPACE, _SUB, _WHITESPACE,
        _LPAREN, _IDENT, _WHITESPACE, _SUB, _WHITESPACE, _IDENT, _DIV, _RESERVED, _RPAREN,
        RX_EOF, ENDT
    };

    struct nfa_match match = {0};
    ck_assert_msg(nfa_start_match(expr, &match, &context), "init match state failed");

    int expected = 0;
    for (int j = 0; (expected = tokens[j]) != ENDT; j++) {
        ck_assert_int_eq(nfa_match(&match), expected);
    }

    free_nfa_match(&match);
}
END_TEST

Suite *nfa_suite()
{
    Suite *s = suite_create("nfa");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_init_with_empty_patterns);
    tcase_add_test(tc_core, test_init_with_invalid_patterns);
    tcase_add_test(tc_core, test_init_with_duplicate_tags);
    tcase_add_test(tc_core, test_init_with_patterns);
    tcase_add_test(tc_core, test_scanner_is_reentrant);
    tcase_add_test(tc_core, test_scans_simple_expression);

    suite_add_tcase(s, tc_core);

    return s;
}

