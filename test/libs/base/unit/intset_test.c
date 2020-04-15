#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <check.h>
#include <base/intset.h>
#include <base/array.h>
#include <base/macros.h>
#include "suites.h"

static void _print_intset(struct intset *set) {
    print_intset(stdout, set);
    printf("\n");
}

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
    ck_assert(has_set ? it->node != NULL : it->node == NULL);
    ck_assert(it->i == bm_iterator);
}

void assert_iter_reset(struct intset_iterator *it) {
    assert_iter(it, 0, true, false, 0);
}

static void setup() {
}

static void teardown() {
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

    free_intset(set);
    int xs[] = { -413000, -2694, -45, 0, 130, 12000, 599906, 523535353 };
    set = sfromlist(xs, SIZEOF(xs));

    struct intset *set2 = sclone(set);
    struct intset_iterator it;

    ck_assert_int_eq(ssize(set), 8);
    ck_assert_int_eq(ssize(set2), 8);

    siterator(set2, &it);
    int x;
    while (snext(&x, &it)) {
        set = sdelete(x, set);
    }

    free_siterator(&it);

    ck_assert_int_eq(ssize(set), 0);

    free_intset(set2);
}
END_TEST

START_TEST(test_sdelete_props_s) {
    int xs[] = {-1258611886,-1258611885,-1258611884};
    int ys[] = {-1258611886,-1258611885};

    struct intset *set2;

    set = sfromlist(xs, SIZEOF(xs));
    set2 = sfromlist(ys, SIZEOF(ys));

    printf("sdelete_props_s:\n");
    print_intset_tree(set);
    set = sdelete(-1258611884, set);
    print_intset_tree(set);

    free_intset(set2);
}
END_TEST

START_TEST(test_snextnode) {
    struct intset_iterator it;

    set = sinsert(9000, set);
    set = sinsert(-9000, set);

    printf("snextnode:\n");
    print_intset_tree(set);

    siterator(set, &it);

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

    siterator(set, &it);
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

    siterator(set, &it);

    assert_iter_reset(&it);

    printf("snext:\n");
    print_intset_tree(set);
    _print_intset(set);

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
    _print_intset(set);
    _print_intset(set2);
    _print_intset(set3);

    struct intset_iterator it;
    siterator(set3, &it);
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

START_TEST(test_sintersection_props) {
    int xs[] = {-638126370,-638126351,-638126350,-638126348};
    int ys[] = {-638126398,-638126397,-638126395,-638126394,-638126392,-638126390,-638126388,-638126385,-638126384,-638126383,-638126382,-638126381,-638126380,-638126377,-638126376,-638126375,-638126373,-638126372,-638126371,-638126370,-638126369,-638126368,-638126362,-638126360,-638126359,-638126358,-638126356,-638126355,-638126354,-638126353,-638126348,-638126347,-638126346,-638126345,-638126343,-638126342,-638126341,-638126339,-638126338,-638126336,-638126335};
    int zs[] = {-638126370,-638126348};

    struct intset *set1, *set2, *set3;

    set = sfromlist(xs, SIZEOF(xs));
    set1 = sfromlist(ys, SIZEOF(ys));
    set2 = sfromlist(zs, SIZEOF(zs));

    set3 = sintersection(set, set1);

    printf("intersection_props:\n");
    printf("expected:\n");
    _print_intset(set2);
    printf("got:\n");
    _print_intset(set3);

    ck_assert(intseteq(set2, set3));

    free_intset(set1);
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
    _print_intset(set);
    _print_intset(set2);
    _print_intset(set3);

    struct intset_iterator it;
    siterator(set3, &it);
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
    _print_intset(set1);
    _print_intset(set2);

    struct intset *set3 = sdifference(set1, set2);

    print_intset_tree(set3);
    _print_intset(set3);

    struct intset_iterator it;
    siterator(set3, &it);
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

START_TEST(test_sdifference_props) {
    int xs[] = {-1231418896,-1231418250,-1231417560,-1231416881};
    int ys[] = {-1231420530,-1231420506,-1231420325,-1231420223,-1231419832,-1231419707,-1231419390,-1231419084,-1231418937,-1231418358,-1231418317,-1231418315,-1231418192,-1231418185,-1231417973,-1231417870,-1231417554,-1231417487,-1231417255,-1231417155,-1231416977,-1231416820,-1231416766,-1231416722,-1231416666,-1231416633};
    int zs[] = {-1231418896,-1231418250,-1231417560,-1231416881};

    struct intset *set1, *set2, *set3;

    set = sfromlist(xs, SIZEOF(xs));
    set1 = sfromlist(ys, SIZEOF(ys));
    set2 = sfromlist(zs, SIZEOF(zs));

    set3 = sdifference(set, set1);

    printf("difference_props:\n");
    printf("expected:\n");
    _print_intset(set2);
    printf("got:\n");
    _print_intset(set3);

    ck_assert(intseteq(set2, set3));

    free_intset(set1);
    free_intset(set2);
    free_intset(set3);
}
END_TEST

START_TEST(test_sdifference_props_xl) {
    int xs[] = {-286147189, -286147148,-286147147,-286147138,-286147101};
    int ys[] = {-286150109,-286150108,-286150067,-286150048,-286149994,-286149922,-286149871,-286149868,-286149741,-286149731,-286149590,-286149551,-286149483,-286149450,-286149302,-286149256,-286149165,-286149104,-286149051,-286149002,-286148991,-286148987,-286148907,-286148850,-286148764,-286148753,-286148742,-286148677,-286148656,-286148578,-286148545,-286148323,-286148262,-286148251,-286148210,-286147969,-286147940,-286147919,-286147917,-286147886,-286147875,-286147788,-286147634,-286147617,-286147452,-286147420,-286147372,-286147327,-286147258,-286147246,-286147214,-286146999,-286146987,-286146887,-286146875,-286146798,-286146796,-286146778,-286146585,-286146495,-286146404,-286146398,-286146189,-286146186,-286146128,-286145995,-286145979,-286145971,-286145909,-286145908,-286145865,-286145861,-286145759,-286145627,-286145561,-286145454,-286145403,-286145361,-286145349,-286145274,-286145214,-286145098,-286145041,-286144984,-286144980,-286144921,-286144780,-286144749,-286144742,-286144590,-286144374,-286144349,-286144257,-286144170,-286144086,-286143820,-286143767,-286143751,-286143738,-286143406,-286143366,-286143361,-286143358,-286143240,-286143226,-286143154,-286143101,-286143096,-286143095,-286143071,-286143029,-286143020,-286143000,-286142929,-286142804,-286142773,-286142656,-286142650,-286142516,-286142375,-286142177,-286142162,-286142114,-286141989,-286141918,-286141770,-286141769,-286141706,-286141504,-286141484,-286141459,-286141447,-286141287,-286141274,-286141269,-286141247,-286141103,-286141078,-286141000,-286140920,-286140862,-286140756,-286140682,-286140629,-286140548,-286140513,-286140483,-286140445,-286140377,-286140357,-286140342,-286140289,-286140260,-286140229,-286140183,-286140068,-286139854,-286139646,-286139454,-286139446,-286139334,-286139327,-286139258,-286139238,-286139139,-286139129,-286139123,-286139069,-286139034,-286139032,-286138803,-286138801,-286138750,-286138745,-286138535,-286138405,-286138333,-286138200,-286138013,-286137992,-286137951,-286137916,-286137858,-286137838,-286137825,-286137564,-286137547,-286137492,-286137141,-286137134,-286137109,-286137083,-286137082,-286136954,-286136931,-286136868,-286136852,-286136818,-286136608,-286136583,-286136573,-286136556,-286136501,-286136459,-286136430,-286136369,-286136315,-286136249,-286136217,-286136175,-286136132,-286136118,-286135839,-286135821,-286135776,-286135764,-286135528,-286135499,-286135364,-286135337,-286135304,-286135266,-286135003,-286134909,-286134897,-286134842,-286134788,-286134731,-286134729,-286134659,-286134622,-286134595,-286134587,-286134533,-286134493,-286134488,-286134416,-286134414,-286134408,-286134382,-286134340,-286134168,-286134076,-286134021,-286133961,-286133879,-286133861,-286133764};
    int zs[] = {-286147189, -286147148,-286147147,-286147138,-286147101};

    struct intset *set1, *set2, *set3;

    set = sfromlist(xs, SIZEOF(xs));
    set1 = sfromlist(ys, SIZEOF(ys));
    set2 = sfromlist(zs, SIZEOF(zs));

    set3 = sdifference(set, set1);

    printf("difference_props_xl:\n");
    printf("expected:\n");
    _print_intset(set2);
    printf("got:\n");
    _print_intset(set3);

    ck_assert(intseteq(set2, set3));

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
    _print_intset(set);
    _print_intset(set2);

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

START_TEST(test_intseteq_0) {
    int xs[] = { 0 };
    struct intset *set2 = NULL;

    set = slistinsert(xs, SIZEOF(xs), set);
    set2 = slistinsert(xs, SIZEOF(xs), set2);

    printf("intseteq_0:\n");
    print_intset_tree(set);
    print_intset_tree(set2);

    ck_assert(intseteq(set, set2));

    free_intset(set2);
}
END_TEST

START_TEST(test_print_intset) {
    add_elements();
    printf("print_intset:\n");
    _print_intset(set);
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
        tcase_add_test(tc_core, test_sdelete_props_s);
        tcase_add_test(tc_core, test_snextnode);
        tcase_add_test(tc_core, test_snextbitmap);
        tcase_add_test(tc_core, test_snext);
        tcase_add_test(tc_core, test_sclone);
        tcase_add_test(tc_core, test_sintersection);
        tcase_add_test(tc_core, test_sintersection_props);
        tcase_add_test(tc_core, test_sunion);
        tcase_add_test(tc_core, test_sdifference);
        tcase_add_test(tc_core, test_sdifference_props);
        tcase_add_test(tc_core, test_sdifference_props_xl);
        tcase_add_test(tc_core, test_sdisjoint);
        tcase_add_test(tc_core, test_stolist);
        tcase_add_test(tc_core, test_intseteq);
        tcase_add_test(tc_core, test_intseteq_0);
        tcase_add_test(tc_core, test_print_intset);
        tcase_add_test(tc_core, test_print_intset_tree);
    }

    suite_add_tcase(s, tc_core);

    return s;
}

