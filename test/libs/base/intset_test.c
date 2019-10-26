#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <check.h>
#include <base/intset.h>
#include <base/array.h>
#include <base/macros.h>
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

    ck_assert_int_eq(ssize(set), 215);

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
    ck_assert(!snull(set));
    ck_assert_int_eq(ssize(set), 5);
    ck_assert_int_eq(streesize(set), 7);
    ck_assert_int_eq(streedepth(set), 3);
}
END_TEST

START_TEST(test_sdelete) {
    add_elements();

    printf("sdelete:\n");
    print_intset_tree(set);
    ck_assert_int_eq(ssize(set), 5);
    set = sdelete(INT_MIN, set);
    print_intset_tree(set);
    ck_assert_int_eq(ssize(set), 4);
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

START_TEST(test_sclone) {
    add_elements();
    printf("sclone:\nset1:\n");
    print_intset_tree(set);

    struct intset *set2 = sclone(set);
    printf("set2:\n");
    print_intset_tree(set2);

    ck_assert(intseteq(set, set2));

    free_intset(set2);

    set2 = sclone(NULL);
    ck_assert(set2 == NULL);

}
END_TEST

START_TEST(test_sintersection) {
    struct intset *set2 = NULL;
    int xs[] = { -413000, -2694, -45, 0, 130, 12000, 599906 };
    int ys[] = { -2694, 0, 599906 };
    set = slistinsert(xs, SIZEOF(xs), set);
    set2 = slistinsert(ys, SIZEOF(ys), set2);

    struct intset *set3 = sintersection(set, set2);
    ck_assert_int_eq(ssize(set3), 3);

    printf("sintersection:\n");
    print_intset(set);
    printf("\n");
    print_intset(set2);
    printf("\n");
    print_intset(set3);
    printf("\n");

    struct intset_iterator it;
    ck_assert(siterator(set3, &it));
    int x;
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, -2694);
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 0);
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 599906);
    ck_assert(!snext(&x, &it));

    ck_assert(intseteq(set2, set3));

    free_siterator(&it);
    free_intset(set2);
    free_intset(set3);
}
END_TEST

START_TEST(test_sunion) {
    struct intset *set2 = NULL;
    int xs[] = { -413000, -2694, -45, 0, 130, 12000 };
    int ys[] = { -2694, 0, 1, 127, 599906  };
    set = slistinsert(xs, SIZEOF(xs), set);
    set2 = slistinsert(ys, SIZEOF(ys), set2);

    struct intset *set3 = sunion(set, set2);
    ck_assert_int_eq(ssize(set3), 9);

    printf("sunion:\n");
    print_intset(set);
    printf("\n");
    print_intset(set2);
    printf("\n");
    print_intset(set3);
    printf("\n");

    struct intset_iterator it;
    ck_assert(siterator(set3, &it));
    int x;
    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, -413000);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, -2694);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, -45);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 0);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 1);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 127);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 130);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 12000);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 599906);

    ck_assert(!snext(&x, &it));

    free_siterator(&it);
    free_intset(set2);
    free_intset(set3);
}
END_TEST

START_TEST(test_sdifference) {
    int xs[] = { -413000, -2694, -45, 0, 130, 12000 };
    int ys[] = { -2694, 0, 1, 0, 1, 0, 1, 127, 127, 599906, 12000 };

    struct intset
        *set1 = sfromlist(xs, SIZEOF(xs)),
        *set2 = sfromlist(ys, SIZEOF(ys));

    printf("sdifference:\n");
    print_intset(set1);
    printf("\n");
    print_intset(set2);
    printf("\n");

    struct intset *set3 = sdifference(set1, set2);

    print_intset_tree(set3);
    print_intset(set3);
    printf("\n");

    struct intset_iterator it;
    ck_assert(siterator(set3, &it));
    int x;

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, -413000);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, -45);

    ck_assert(snext(&x, &it));
    ck_assert_int_eq(x, 130);

    ck_assert(!snext(&x, &it));

    free_siterator(&it);
    free_intset(set1);
    free_intset(set2);
    free_intset(set3);
}
END_TEST

START_TEST(test_sdisjoint) {
    struct intset *set2 = NULL;
    set = sinsert(-90, set);
    set = sinsert(0, set);
    set = sinsert(900, set);
    set2 = sinsert(-89, set2);
    set2 = sinsert(1, set2);
    set2 = sinsert(899, set2);
    set2 = sinsert(901, set2);

    printf("sdisjoint:\n");
    print_intset(set);
    printf("\n");
    print_intset(set2);
    printf("\n");

    ck_assert(sdisjoint(set, set2));

    free_intset(set2);
    set2 = NULL;

    set2 = sinsert(-90, set2);

    ck_assert(!sdisjoint(set, set2));
    free_intset(set2);
}
END_TEST

START_TEST(test_stolist) {
    add_elements();

    int *xs = stolist(set);

    ck_assert_int_eq(xs[0], INT_MIN);
    ck_assert_int_eq(xs[1], -50153);
    ck_assert_int_eq(xs[2], 0);
    ck_assert_int_eq(xs[3], 63);
    ck_assert_int_eq(xs[4], INT_MAX);

    free(xs);
}
END_TEST

START_TEST(test_intseteq) {
    struct intset *set2 = NULL;

    for (int i = -100000; i < 100000; i += 1000) {
        set = sinsert(i, set);
        set2 = sinsert(i, set2);
    }

    ck_assert_int_eq(ssize(set), 200);
    ck_assert_int_eq(ssize(set2), 200);
    ck_assert_msg(intseteq(set, set2), "set 1 and set 2 are not equal");

    free_intset(set2);
    set2 = NULL;

    ck_assert_msg(!intseteq(set, set2), "set 1 and 2 are equal");

    free_intset(set2);
}
END_TEST

START_TEST(test_print_intset) {
    add_elements();
    printf("print_intset:\n");
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
        tcase_add_test(tc_core, test_sdelete);
        tcase_add_test(tc_core, test_snextnode);
        tcase_add_test(tc_core, test_snextbitmap);
        tcase_add_test(tc_core, test_snext);
        tcase_add_test(tc_core, test_sclone);
        tcase_add_test(tc_core, test_sintersection);
        tcase_add_test(tc_core, test_sunion);
        tcase_add_test(tc_core, test_sdifference);
        tcase_add_test(tc_core, test_sdisjoint);
        tcase_add_test(tc_core, test_stolist);
        tcase_add_test(tc_core, test_intseteq);
        tcase_add_test(tc_core, test_print_intset);
        tcase_add_test(tc_core, test_print_intset_tree);
    }

    suite_add_tcase(s, tc_core);

    return s;
}

