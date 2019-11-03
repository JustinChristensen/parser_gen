#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <check.h>
#include <base/array.h>
#include <base/macros.h>
#include "test_base.h"

struct array *arr = NULL;

static void setup() {
}

static void teardown() {
    free_array(arr);
    arr = NULL;
}

START_TEST(test_adel) {
    arr = init_array(sizeof(int), 1, LINEAR, 1);

    int x = 9;
    apush(&x, arr);
    x = 6;
    apush(&x, arr);
    x = 3;
    apush(&x, arr);

    // 9 6 3

    ck_assert_int_eq(asize(arr), 3);
    ck_assert_int_eq(arr->i, 3);

    adel(aptr(1, arr), arr); // delete 6

    // 9 3
    ck_assert_int_eq(asize(arr), 2);
    ck_assert_int_eq(arr->i, 2);
    at(&x, 0, arr);
    ck_assert_int_eq(x, 9);
    at(&x, 1, arr);
    ck_assert_int_eq(x, 3);

    adel(aptr(0, arr), arr);
    // 3

    ck_assert_int_eq(asize(arr), 1);
    ck_assert_int_eq(arr->i, 1);
    at(&x, 0, arr);
    ck_assert_int_eq(x, 3);

    adel(aptr(0, arr), arr);
    //

    ck_assert_int_eq(asize(arr), 0);
    ck_assert_int_eq(arr->i, 0);
}
END_TEST

Suite *array_suite()
{
    Suite *s = suite_create("array");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_adel);

    suite_add_tcase(s, tc_core);

    return s;
}

