#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <check.h>
#include <base/btree.h>
#include <base/macros.h>
#include <base/string.h>
#include <base/random.h>
#include "test_base.h"

struct bin *node = NULL;

static void setup() {
}

static void teardown() {
    free_btree(node);
    node = NULL;
}

char *A = "A", *B = "B", *C = "C",
     *D = "D", *E = "E", *F = "F",
     *G = "G", *H = "H", *I = "I";

void build_wiki_tree() {
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) F, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) B, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) A, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) D, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) C, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) E, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) G, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) I, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
    node = btinsert((void *) H, CMPFN strcmp, NULL, node);
    btinvariants(node, true, CMPFN strcmp);
}

START_TEST(test_insert_steps) {
    struct bin *node2 = NULL;
    char *keys[] = { A, B, C, D, E, F, G, H, I };

    // printf("---\n");
    for (int i = 1; i < SIZEOF(keys) + 1; i++) {
        for (int j = 0; j < i; j++) {
            node2 = btinsert(keys[j], CMPFN strcmp, NULL, node2);
        }

        // print_btree(PRINTFN printstr, node2);
        // printf("---\n");
        btinvariants(node2, true, CMPFN strcmp);
        free_btree(node2);
        node2 = NULL;
    }
}
END_TEST

static int intcmp(int const *a, int const *b) {
    return *a - *b;
}

// static void printint(int const *a) {
//     printf("%d", *a);
// }

START_TEST(insert_ascending) {
    char *max_inserts = getenv("MAX_INSERTS");
    int num_inserts = 100;

    if (max_inserts) {
        num_inserts = atoi(max_inserts);
    }

    int n = randr(10, num_inserts);
    int *keys = calloc(n, sizeof *keys);

    for (int i = 0; i < n; i++) {
        keys[i] = i;
        node = btinsert(&keys[i], CMPFN intcmp, NULL, node);
    }

    size_t s = btsize(node);
    printf("size: %lu, height: %lu\n", s, btdepth(node));
    ck_assert_int_eq(n, s);
    btinvariants(node, true, CMPFN intcmp);
    // print_btree(PRINTFN printint, node);
    free(keys);
}
END_TEST

START_TEST(test_stats) {
    build_wiki_tree();

    ck_assert_int_eq(btsize(node), 9);
    ck_assert_int_eq(btdepth(node), 4);
    printf("sizeof(struct bin): %lu\n", sizeof(struct bin));
    print_btree(PRINTFN printstr, node);
}
END_TEST

START_TEST(test_equality) {
    struct bin *node2 = NULL;

    struct assoc as[] = {
        { "bar", NULL },
        { "baz", NULL },
        { "foo", NULL },
        { "quux", NULL }
    };

    node = btfromlist(as, SIZEOF(as), CMPFN strcmp);

    struct assoc as2[] = {
        { "foo", NULL },
        { "bar", NULL },
        { "baz", NULL },
        { "quux", NULL }
    };

    node2 = btfromlist(as2, SIZEOF(as2), CMPFN strcmp);

    ck_assert(btree_eq(EQFN streq, NULL, node, node2));

    free_btree(node2);
}
END_TEST

START_TEST(test_simple_delete) {
    struct assoc as[] = {
        { "A", NULL },
        { "B", NULL },
        { "C", NULL },
        { "D", NULL },
        { "E", NULL },
        { "@", NULL }
    };

    size_t n = SIZEOF(as);
    node = btfromlist(as, n, CMPFN strcmp);
    printf("test_simple_delete:\n");
    print_btree(PRINTFN printstr, node);
    printf("---\n");
    ck_assert_int_eq(n, btsize(node));

    node = btdelete("B", CMPFN strcmp, node);
    print_btree(PRINTFN printstr, node);
    printf("---\n");
    ck_assert_int_eq(n - 1, btsize(node));

    node = btdelete("D", CMPFN strcmp, node);
    print_btree(PRINTFN printstr, node);
    printf("---\n");
    ck_assert_int_eq(n - 2, btsize(node));

    node = btdelete("C", CMPFN strcmp, node);
    print_btree(PRINTFN printstr, node);
    printf("---\n");
    ck_assert_int_eq(n - 3, btsize(node));

    node = btdelete("B", CMPFN strcmp, node);
    print_btree(PRINTFN printstr, node);
    printf("---\n");
    ck_assert_int_eq(n - 3, btsize(node));

    node = btdelete("E", CMPFN strcmp, node);
    print_btree(PRINTFN printstr, node);
    printf("---\n");
    ck_assert_int_eq(n - 4, btsize(node));

    node = btdelete("@", CMPFN strcmp, node);
    print_btree(PRINTFN printstr, node);
    ck_assert_int_eq(n - 5, btsize(node));
}
END_TEST

// START_TEST(test_delete) {
//     struct assoc as[] = {
//         { "", NULL },
//         { "\nULsD/", NULL }
//     };
//
//     node = btfromlist(as, SIZEOF(as), CMPFN strcmp);
//
//     struct assoc as2[] = {
//         { "", NULL }
//     };
//
//     struct bin *node2 = btfromlist(as2, SIZEOF(as2), CMPFN strcmp);
//
//     print_btree(PRINTFN printstr, node);
//
//     node = btdelete("\nULsD/", CMPFN strcmp, node);
//
//     print_btree(PRINTFN printstr, node);
//     print_btree(PRINTFN printstr, node2);
//
//     ck_assert(btree_eq(EQFN streq, NULL, node, node2));
//
//     free_btree(node2);
// }
// END_TEST

START_TEST(test_preorder_traversal) {
    build_wiki_tree();

    struct btree_iter it;
    btree_iter(PRE, node, &it);

    char *keys[] = { D, B, A, C, F, E, H, G, I, NULL };

    char **k = keys;
    while (*k) {
        ck_assert_str_eq(nodekey(btnext(&it)), *k);
        k++;
    }

    free_btree_iter(&it);
}
END_TEST

START_TEST(test_inorder_traversal) {
    build_wiki_tree();

    struct btree_iter it;
    btree_iter(IN, node, &it);

    char *keys[] = { A, B, C, D, E, F, G, H, I, NULL };

    char **k = keys;
    while (*k) {
        ck_assert_str_eq(nodekey(btnext(&it)), *k);
        k++;
    }

    free_btree_iter(&it);
}
END_TEST

START_TEST(test_postorder_traversal) {
    build_wiki_tree();

    struct btree_iter it;
    btree_iter(POST, node, &it);

    char *keys[] = { A, C, B, E, G, I, H, F, D, NULL };

    char **k = keys;
    while (*k) {
        ck_assert_str_eq(nodekey(btnext(&it)), *k);
        k++;
    }

    free_btree_iter(&it);
}
END_TEST

Suite *btree_suite()
{
    Suite *s = suite_create("btree");

    TCase *tc_core = tcase_create("core"),
          *tc_random = tcase_create("random");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_stats);
    tcase_add_test(tc_core, test_insert_steps);
    tcase_add_test(tc_core, test_equality);
    tcase_add_test(tc_core, test_simple_delete);
    // tcase_add_test(tc_core, test_delete);
    tcase_add_test(tc_core, test_preorder_traversal);
    tcase_add_test(tc_core, test_inorder_traversal);
    tcase_add_test(tc_core, test_postorder_traversal);

    tcase_add_test(tc_random, insert_ascending);

    suite_add_tcase(s, tc_core);
    suite_add_tcase(s, tc_random);

    return s;
}

