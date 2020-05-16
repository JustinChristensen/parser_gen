#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <check.h>
#include <base/rbtree.h>
#include <base/macros.h>
#include <base/string.h>
#include <base/debug.h>
#include <base/random.h>
#include <base/ord.h>
#include "suites.h"

#define debug(...) debug_ns("rbtree", __VA_ARGS__);

struct rb_node *node = NULL;

static void setup() {
}

static void teardown() {
    free_rbtree(node);
    node = NULL;
}

static void print_strkey(FILE *handle, void const *key, void const *val) {
    UNUSED(val);
    fprintf(handle, "%s", key);
}

static void _print_rbtree(struct rb_node *tree) {
    if (debug_is("rbtree"))
        print_rbtree(stderr, print_strkey, tree);
}

char *A = "A", *B = "B", *C = "C",
     *D = "D", *E = "E", *F = "F",
     *G = "G", *H = "H", *I = "I";

void build_wiki_tree() {
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) F, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) B, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) A, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) D, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) C, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) E, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) G, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) I, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
    node = rbinsert((void *) H, CMPFN strcmp, NULL, node);
    rbinvariants(node, true, CMPFN strcmp);
}

START_TEST(test_insert_steps) {
    struct rb_node *node2 = NULL;
    char *keys[] = { A, B, C, D, E, F, G, H, I };

    for (unsigned long i = 1; i < SIZEOF(keys) + 1; i++) {
        for (unsigned long j = 0; j < i; j++) {
            node2 = rbinsert(keys[j], CMPFN strcmp, NULL, node2);
        }

        rbinvariants(node2, true, CMPFN strcmp);
        free_rbtree(node2);
        node2 = NULL;
    }
}
END_TEST

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
        node = rbinsert(&keys[i], CMPFN intcmp, NULL, node);
    }

    size_t s = rbsize(node);
    debug("size: %lu, height: %lu\n", s, rbdepth(node));
    ck_assert_int_eq(n, s);
    rbinvariants(node, true, CMPFN intcmp);
    free(keys);
}
END_TEST

START_TEST(test_stats) {
    build_wiki_tree();

    ck_assert_int_eq(rbsize(node), 9);
    ck_assert_int_eq(rbdepth(node), 4);
    debug("sizeof(struct rb_node): %lu\n", sizeof(struct rb_node));
    _print_rbtree(node);
}
END_TEST

START_TEST(test_equality) {
    struct rb_node *node2 = NULL;

    struct rb_assoc as[] = {
        { "bar", NULL },
        { "baz", NULL },
        { "foo", NULL },
        { "quux", NULL }
    };

    node = rbfromlist(as, SIZEOF(as), CMPFN strcmp);

    struct rb_assoc as2[] = {
        { "foo", NULL },
        { "bar", NULL },
        { "baz", NULL },
        { "quux", NULL }
    };

    node2 = rbfromlist(as2, SIZEOF(as2), CMPFN strcmp);

    ck_assert(rbtree_eq(EQFN streq, NULL, node, node2));

    free_rbtree(node2);
}
END_TEST

// START_TEST(test_delete) {
//     struct rb_assoc as[] = {
//         { "", NULL },
//         { "\nULsD/", NULL }
//     };
//
//     node = rbfromlist(as, SIZEOF(as), CMPFN strcmp);
//
//     struct rb_assoc as2[] = {
//         { "", NULL }
//     };
//
//     struct rb_node *node2 = rbfromlist(as2, SIZEOF(as2), CMPFN strcmp);
//
//     debug("test_delete\n");
//     _print_rbtree(node);
//
//     node = rbdelete("\nULsD/", CMPFN strcmp, node);
//
//     _print_rbtree(node);
//     _print_rbtree(node2);
//
//     ck_assert(rbtree_eq(EQFN streq, NULL, node, node2));
//
//     free_rbtree(node2);
//
//     struct rb_assoc as3[] = {
//         { "A", NULL },
//         { "B", NULL },
//         { "C", NULL },
//         { "D", NULL }
//     };
//
//     node2 = rbfromlist(as3, SIZEOF(as3), CMPFN strcmp);
//     node2 = rbdelete("D", CMPFN strcmp, node2);
//
//     _print_rbtree(node2);
//
//     free_rbtree(node2);
// }
// END_TEST

START_TEST(test_preorder_traversal) {
    build_wiki_tree();

    struct rb_iter it;
    rb_iter(RB_PRE, node, &it);

    char *keys[] = { D, B, A, C, F, E, H, G, I, NULL };

    char **k = keys;
    while (*k) {
        ck_assert_str_eq(rbkey(rbnext(&it)), *k);
        k++;
    }

    free_rb_iter(&it);
}
END_TEST

START_TEST(test_inorder_traversal) {
    build_wiki_tree();

    struct rb_iter it;
    rb_iter(RB_IN, node, &it);

    char *keys[] = { A, B, C, D, E, F, G, H, I, NULL };

    char **k = keys;
    while (*k) {
        ck_assert_str_eq(rbkey(rbnext(&it)), *k);
        k++;
    }

    free_rb_iter(&it);
}
END_TEST

START_TEST(test_postorder_traversal) {
    build_wiki_tree();

    struct rb_iter it;
    rb_iter(RB_POST, node, &it);

    char *keys[] = { A, C, B, E, G, I, H, F, D, NULL };

    char **k = keys;
    while (*k) {
        ck_assert_str_eq(rbkey(rbnext(&it)), *k);
        k++;
    }

    free_rb_iter(&it);
}
END_TEST

Suite *rbtree_suite()
{
    Suite *s = suite_create("rbtree");

    TCase *tc_core = tcase_create("core"),
          *tc_random = tcase_create("random");

    tcase_add_checked_fixture(tc_core, setup, teardown);

    tcase_add_test(tc_core, test_stats);
    tcase_add_test(tc_core, test_insert_steps);
    tcase_add_test(tc_core, test_equality);
    tcase_add_test(tc_core, test_preorder_traversal);
    tcase_add_test(tc_core, test_inorder_traversal);
    tcase_add_test(tc_core, test_postorder_traversal);

    tcase_add_test(tc_random, insert_ascending);

    suite_add_tcase(s, tc_core);
    suite_add_tcase(s, tc_random);

    return s;
}

