#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <check.h>
#include <base/btree.h>
#include <base/macros.h>
#include <base/string.h>
#include "test_base.h"

struct btnode *node = NULL;

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
    node = btinsert((void *) F, CMPFN strcmp, NULL, node);
    node = btinsert((void *) B, CMPFN strcmp, NULL, node);
    node = btinsert((void *) A, CMPFN strcmp, NULL, node);
    node = btinsert((void *) D, CMPFN strcmp, NULL, node);
    node = btinsert((void *) C, CMPFN strcmp, NULL, node);
    node = btinsert((void *) E, CMPFN strcmp, NULL, node);
    node = btinsert((void *) G, CMPFN strcmp, NULL, node);
    node = btinsert((void *) I, CMPFN strcmp, NULL, node);
    node = btinsert((void *) H, CMPFN strcmp, NULL, node);

    print_btree(PRINTFN printstr, node);
}

START_TEST(test_preorder_traversal) {
    build_wiki_tree();

    struct btree_iter it;
    btree_iter(PRE, node, &it);

    char *keys[] = { F, B, A, D, C, E, G, I, H, NULL };

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

    char *keys[] = { A, C, E, D, B, H, I, G, F, NULL };

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

    TCase *tc_core = tcase_create("core");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_preorder_traversal);
    tcase_add_test(tc_core, test_inorder_traversal);
    tcase_add_test(tc_core, test_postorder_traversal);

    suite_add_tcase(s, tc_core);

    return s;
}

