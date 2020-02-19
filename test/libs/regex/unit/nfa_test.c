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
        print_regex_error(nfa_error(&context));
    }
}

START_TEST(test_init_with_empty_patterns) {
    bool success = nfa_context(&context, PATTERNS {
        END_PATTERNS
    });

    print_if_error();

    ck_assert_msg(success, "init with empty patterns failed");
}
END_TEST

START_TEST(test_init_with_invalid_patterns) {
    ck_assert_msg(nfa_context(&context, PATTERNS {
        { 0, NULL, "{5}" },
        END_PATTERNS
    }) == false, "init with invalid patterns succeeded");

    ck_assert(nfa_error(&context).type == SYNTAX_ERROR);
}
END_TEST

START_TEST(test_init_with_duplicate_tags) {
    ck_assert_msg(nfa_context(&context, PATTERNS {
        { 0, "abc", "" },
        { 1, "abc", "" },
        END_PATTERNS
    }) == false, "init with duplicate tags succeeded");

    ck_assert(nfa_error(&context).type == TAG_EXISTS);
}
END_TEST

START_TEST(test_init_with_patterns) {
    bool success = nfa_context(&context, PATTERNS {
        RE_ALNUM_(TAG_ONLY), RE_EOF(0),
        { 1, "abc", "a|b|c" },
        { 2, "a!", "a[?!]" },
        { 3, NULL, "{alnum_}{5}" },
        { 4, "e's", "([0-7]|u)|e+" },
        { 5, "aa",  ".." },
        { 6, "n followed by optional binary operator",  "[0-9]+[\t ]*(\\+|-|\\*|/)" },
        END_PATTERNS
    });

    print_if_error();

    ck_assert_msg(success, "init with valid patterns failed");
}
END_TEST

START_TEST(test_scans_simple_expression) {
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
    tcase_add_test(tc_core, test_scans_simple_expression);

    suite_add_tcase(s, tc_core);

    return s;
}

