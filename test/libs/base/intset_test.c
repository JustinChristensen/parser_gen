#include <check.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
#include <base/intset.h>
#include "test_base.h"

struct intset *set = NULL;

void setup() {
}

void teardown() {
    free_intset(set);
    set = NULL;
}

void add_elements() {
    set = isinsert(0, set);
    set = isinsert(63, set);
    set = isinsert(-50153, set);
    set = isinsert(INT_MIN, set);
    set = isinsert(INT_MAX, set);
}

START_TEST(test_null_intset) {
    ck_assert_msg(isnull(set), "set is null");
    ck_assert_msg(issize(set) == 0, "set has no elements");
    ck_assert_msg(istreesize(set) == 0, "set has no nodes");
    ck_assert_msg(istreedepth(set) == 0, "set has no depth");
}
END_TEST

START_TEST(test_isinsert) {
    add_elements();
    ck_assert_msg(isnull(set) == false, "set is not null");
    ck_assert_msg(issize(set) == 5, "set has five elements");
    ck_assert_msg(istreesize(set) == 7, "set has 7 nodes");
    ck_assert_msg(istreedepth(set) == 3, "set has depth of 3");
}
END_TEST

START_TEST(test_print_intset_tree) {
    add_elements();
    print_intset_tree(set);
}
END_TEST

Suite *intset_suite()
{
    Suite *s = suite_create("intset");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_null_intset);
    tcase_add_test(tc_core, test_isinsert);
    tcase_add_test(tc_core, test_print_intset_tree);
    suite_add_tcase(s, tc_core);

    return s;
}

