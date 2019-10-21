#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <check.h>
#include <base/intset.h>
#include <base/array.h>
#include "test_base.h"

struct intset *set = NULL;

void assert_branch(struct intset const *set) {
    ck_assert_msg(set->left, "set node is a branch");
}

void assert_iter(
    struct intset_iterator *it,
    size_t stack_size,
    bool is_root,
    bool has_set,
    int bm_iterator
) {
    ck_assert(asize(it->stack) == stack_size);
    ck_assert(it->at_root == is_root);
    ck_assert(has_set ? it->set != NULL : it->set == NULL);
    ck_assert(it->i == bm_iterator);
}

void assert_iter_reset(struct intset_iterator *it) {
    assert_iter(it, 0, true, false, 0);
}

void setup() {
}

void teardown() {
    free_intset(set);
    set = NULL;
}

void add_elements() {
    set = sinsert(0, set);
    set = sinsert(63, set);
    set = sinsert(63, set);
    set = sinsert(-50153, set);
    set = sinsert(INT_MIN, set);
    set = sinsert(INT_MAX, set);
}

START_TEST(test_null_intset) {
    ck_assert_msg(snull(set), "set is null");
    ck_assert_msg(ssize(set) == 0, "set has no elements");
    ck_assert_msg(streesize(set) == 0, "set has no nodes");
    ck_assert_msg(streedepth(set) == 0, "set has no depth");
}
END_TEST

START_TEST(test_selem) {
    int key_starts[] = { -413000, -2694, 0, 130, 12000 };
    size_t n = sizeof key_starts / sizeof key_starts[0];
    char msgbuf[BUFSIZ] = "";

    int i;
    for (i = 0; i < n; i++) {
        int ks = key_starts[i];
        for (int j = ks; j < ks + 128; j+=3) {
            set = sinsert(j, set);
        }
    }

    // print_intset_tree(set);
    sprintf(msgbuf, "expected 215 elements, got %lu", ssize(set));
    ck_assert_msg(ssize(set) == 215, msgbuf);

    for (i = 0; i < n; i++) {
        int ks = key_starts[i];
        for (int j = key_starts[i]; j < ks + 128; j+=3) {
            sprintf(msgbuf, "%d is not an element of the set", j);
            ck_assert_msg(selem(j, set), msgbuf);
        }
    }

    ck_assert_msg(!selem(129, set), "129 is not an element of the set");
    ck_assert_msg(!selem(-413001, set), "-413001 is not an element of the set");
}
END_TEST

START_TEST(test_sinsert) {
    add_elements();
    ck_assert_msg(snull(set) == false, "set is not null");
    ck_assert_msg(ssize(set) == 5, "set has 5 elements");
    ck_assert_msg(streesize(set) == 7, "set has 7 nodes");
    ck_assert_msg(streedepth(set) == 3, "set has depth of 4");
}
END_TEST

START_TEST(test_snextnode) {
    struct intset_iterator it;

    set = sinsert(9000, set);
    set = sinsert(-9000, set);

    printf("snextnode:\n");
    print_intset_tree(set);

    ck_assert(siterator(set, &it));

    // twice
    for (int i = 0; i < 2; i++) {
        struct intset const *x;
        size_t n = 0;

        // assert that the iterator is reset
        assert_iter_reset(&it);

        while (snextnode(&x, &it)) n++;

        // assert we've visited each node
        // one branch, and two leaf nodes
        ck_assert(n == 3);
    }

    free_siterator(&it);
}
END_TEST

START_TEST(test_snextbitmap) {
    set = sinsert(5, set);
    struct intset_iterator it;
    struct intset const *leaf = NULL;
    int x;

    ck_assert(siterator(set, &it));
    ck_assert(snextleaf(&leaf, &it));

    ck_assert_msg(snextbitmap(&x, &it), "integer not found in bitmap"); ;
    ck_assert_int_eq(x, 5);

    ck_assert_msg(snextbitmap(&x, &it) == false, "snextbitmap returned true after reaching the end");
    ck_assert(snextleaf(&leaf, &it) == false);
    assert_iter_reset(&it);

    add_elements();

    // more intensive test using the leaf iterator
    printf("snextleaf/snextbitmap\n");
    while (snextleaf(&leaf, &it)) {
        while (snextbitmap(&x, &it)) {
            printf("%d\n", x);
        }
    }

    assert_iter_reset(&it);

    free_siterator(&it);
}
END_TEST

START_TEST(test_snext) {
    int ints[] = { 5, 129, 603 };
    size_t n = sizeof ints / sizeof ints[0];
    set = slistinsert(ints, n, set);

    struct intset_iterator it;

    ck_assert(siterator(set, &it));

    assert_iter_reset(&it);

    printf("snext:\n");
    print_intset_tree(set);
    print_intset(set);
    printf("\n");

    int x;
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 5);
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 129);
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 603);
    ck_assert(!snext(&x, &it));

    assert_iter_reset(&it);

    free_siterator(&it);
}
END_TEST

START_TEST(test_print_intset) {
    add_elements();
    print_intset(set);
    printf("\n");
}
END_TEST

START_TEST(test_print_intset_tree) {
    add_elements();
    printf("print_intset_tree:\n");
    print_intset_tree(set);
}
END_TEST

START_TEST(test_print_matching_prefixes) {
    set = sinsert(0b01110000101000, set);
    set = sinsert(0b01110000101011, set);
    set = sinsert(0b01100000101111, set);
    printf("test_print_matching_prefixes:\n");
    print_intset_tree(set);
}
END_TEST

Suite *intset_suite()
{
    Suite *s = suite_create("intset");
    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    if (getenv(RUN_DIAGNOSTICS)) {
        tcase_add_test(tc_core, test_print_matching_prefixes);
    } else {
        tcase_add_test(tc_core, test_null_intset);
        tcase_add_test(tc_core, test_selem);
        tcase_add_test(tc_core, test_sinsert);
        tcase_add_test(tc_core, test_snextnode);
        tcase_add_test(tc_core, test_snextbitmap);
        tcase_add_test(tc_core, test_snext);
        tcase_add_test(tc_core, test_print_intset);
        tcase_add_test(tc_core, test_print_intset_tree);
    }

    suite_add_tcase(s, tc_core);

    return s;
}

