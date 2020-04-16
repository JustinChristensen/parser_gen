#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <check.h>
#include <base/base.h>
#include <base/bitset.h>
#include <base/debug.h>
#include <base/macros.h>
#include "suites.h"

#define debug(...) debug_ns("bitset", __VA_ARGS__);

#define N 129

static void _print_bitset_bits(struct bitset const *s) {
    if (debug_is("bitset")) {
        print_bitset_bits(stderr, s);
        fprintf(stderr, "\n");
    }
}

static void _print_bitset(struct bitset const *s) {
    if (debug_is("bitset")) {
        print_bitset(stderr, s);
        fprintf(stderr, "\n");
    }
}

static struct bitset *set = NULL;

static void setup() {
    set = bitset(N);
    ck_assert(set != NULL);
    ck_assert(set->nwords = 3);
}

static void teardown() {
    free(set);
}

START_TEST(test_bsins_bsdel) {
    debug("test_bsins_bsdel:\n");

    unsigned elems[] = { 0, 1, 14, 63, 64 };

    for (int i = 0; i < SIZEOF(elems); i++) {
        _print_bitset_bits(set);
        ck_assert_int_eq(bssize(set), i);
        bsins(elems[i], set);
    }

    bsins(1, set);
    _print_bitset_bits(set);
    ck_assert_int_eq(bssize(set), 5);

    bsdel(1, set);
    _print_bitset_bits(set);
    ck_assert_int_eq(bssize(set), 4);

    bsdel(64, set);
    _print_bitset_bits(set);
    ck_assert_int_eq(bssize(set), 3);

    bsdel(64, set);
    _print_bitset_bits(set);
    ck_assert_int_eq(bssize(set), 3);
}
END_TEST

START_TEST(test_bsunion) {
    debug("test_bsunion:\n");

    struct bitset
        *s = bitset(N),
        *t = bitset(N),
        *u = bitset(N),
        *v = bitset(N);

    unsigned
        ws[] = { 3, 3, 19, 19, 47 },
        xs[] = { 1, 4, 64, 19, 18 },
        ys[] = { 37 },
        zs[] = { 1, 3, 4, 18, 19, 37, 47, 64 };

    bsinsarr(s, SIZEOF(ws), ws);
    bsinsarr(t, SIZEOF(xs), xs);
    bsinsarr(u, SIZEOF(ys), ys);
    bsinsarr(v, SIZEOF(zs), zs);

    bsunion(s, t, u);
    _print_bitset(s);

    ck_assert(bseq(s, v));

    freel(s, t, u, v);
}
END_TEST

START_TEST(test_bsintersect) {
    debug("test_bsintersect:\n");

    struct bitset
        *s = bitset(N),
        *t = bitset(N),
        *u = bitset(N),
        *v = bitset(N);

    unsigned
        ws[] = { 3, 3, 19, 19, 47 },
        xs[] = { 0, 3, 47, 64, 88 },
        ys[] = { 1, 3 },
        zs[] = { 3 };

    bsinsarr(s, SIZEOF(ws), ws);
    bsinsarr(t, SIZEOF(xs), xs);
    bsinsarr(u, SIZEOF(ys), ys);
    bsinsarr(v, SIZEOF(zs), zs);

    bsintersect(s, t, u);
    _print_bitset(s);

    ck_assert(bseq(s, v));

    freel(s, t, u, v);
}
END_TEST

START_TEST(test_bsdifference) {
    debug("test_bsdifference:\n");

    struct bitset
        *s = bitset(N),
        *t = bitset(N),
        *u = bitset(N),
        *v = bitset(N);

    unsigned
        ws[] = { 1, 1, 2, 3, 5, 9, 71 },
        xs[] = { 1, 5 },
        ys[] = { 3, 5 },
        zs[] = { 2, 9, 71 };

    bsinsarr(s, SIZEOF(ws), ws);
    bsinsarr(t, SIZEOF(xs), xs);
    bsinsarr(u, SIZEOF(ys), ys);
    bsinsarr(v, SIZEOF(zs), zs);

    bsdifference(s, t, u);
    _print_bitset(s);

    ck_assert(bseq(s, v));

    freel(s, t, u, v);
}
END_TEST

START_TEST(test_bsdisjoint) {
    debug("test_bsdisjoint:\n");

    struct bitset
        *s = bitset(N),
        *t = bitset(N),
        *u = bitset(N);

    unsigned
        ws[] = { 1, 1, 2, 3, 5, 9, 71 },
        xs[] = { 4, 70 },
        ys[] = { 6, 10, 70 };

    bsinsarr(s, SIZEOF(ws), ws);
    bsinsarr(t, SIZEOF(xs), xs);
    bsinsarr(u, SIZEOF(ys), ys);

    ck_assert(bsdisjoint(s, t));
    ck_assert(!bsdisjoint(t, u));

    freel(s, t, u);
}
END_TEST

Suite *bitset_suite()
{
    Suite *s = suite_create("bitset");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_bsins_bsdel);
    tcase_add_test(tc_core, test_bsunion);
    tcase_add_test(tc_core, test_bsintersect);
    tcase_add_test(tc_core, test_bsdifference);
    tcase_add_test(tc_core, test_bsdisjoint);

    suite_add_tcase(s, tc_core);

    return s;
}

