#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <check.h>
#include <base/bitset.h>
#include <base/macros.h>
#include "suites.h"

static void _print_bitset_bits(FILE *handle, struct bitset const *s) {
    print_bitset_bits(handle, s);
    printf("\n");
}

static struct bitset *set = NULL;

static void setup() {
    set = bitset(65);
    ck_assert(set != NULL);
    ck_assert(set->nwords = 2);
}

static void teardown() {
    free(set);
}

START_TEST(test_bsins_bsdel) {
    unsigned elems[] = { 0, 1, 14, 63, 64 };

    for (int i = 0; i < SIZEOF(elems); i++) {
        _print_bitset_bits(stdout, set);
        ck_assert_int_eq(bssize(set), i);
        bsins(elems[i], set);
    }

    bsins(1, set);
    _print_bitset_bits(stdout, set);
    ck_assert_int_eq(bssize(set), 5);

    bsdel(1, set);
    _print_bitset_bits(stdout, set);
    ck_assert_int_eq(bssize(set), 4);

    bsdel(64, set);
    _print_bitset_bits(stdout, set);
    ck_assert_int_eq(bssize(set), 3);

    bsdel(64, set);
    _print_bitset_bits(stdout, set);
    ck_assert_int_eq(bssize(set), 3);
}
END_TEST

Suite *bitset_suite()
{
    Suite *s = suite_create("bitset");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_bsins_bsdel);

    suite_add_tcase(s, tc_core);

    return s;
}

