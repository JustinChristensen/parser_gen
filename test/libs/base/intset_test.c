#include <check.h>
#include <stdbool.h>
#include <base/intset.h>
#include "test_base.h"

START_TEST(test_create_intset) {
    ck_assert(true);
}
END_TEST

Suite *intset_suite()
{
    Suite *s = suite_create("intset");
    TCase *tc_core = tcase_create("core");

    // tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_create_intset);
    suite_add_tcase(s, tc_core);

    return s;
}

