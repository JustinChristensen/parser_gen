#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <regex/nfa.h>
#include "suites.h"

static void setup() {
}

static void teardown() {
}

START_TEST(test_init_context_with_patterns) {
}
END_TEST

Suite *intset_suite()
{
    Suite *s = suite_create("nfa");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_init_context_with_patterns);

    suite_add_tcase(s, tc_core);

    return s;
}

