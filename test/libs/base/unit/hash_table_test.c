#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <ctype.h>
#include <check.h>
#include <base/hash_table.h>
#include <base/random.h>
#include <base/macros.h>
#include "suites.h"

struct hash_table *table = NULL;

static void int_table() {
    table = hash_table(sizeof (int));
    ck_assert(table != NULL);
}

static void string_table() {
    table = hash_table(sizeof (char *));
    ck_assert(table != NULL);
}

static void setup() {
}

static void teardown() {
    free_hash_table(table);
    table = NULL;
}

START_TEST(test_basic_insert) {
    int_table();
    char c;
    int i;

    for (c = 'a', i = 3; c < 'h'; c++, i += 2) {
        char key[] = { c, '\0' };
        htinsert_i(key, i, table);
    }

    for (c = 'a'; c < 'h'; c++) {
        char key[] = { c, '\0' };
        ck_assert(htcontains(key, table));
    }

    print_hash_table(print_entry_int, table);
}
END_TEST

START_TEST(test_keys) {
    int_table();
    htinsert_i("foo", 99, table);
    htinsert_i("bar", 99, table);
    htinsert_i("baz", 99, table);

    char **keys = htkeys(table);

    printf("htkeys:\n");
    unsigned int entries = htentries(table);

    for (int i = 0; i < entries; i++) {
        printf("%s ", keys[i]);
        ck_assert(htdelete(keys[i], table));
    }
    printf("\n");

    free(keys);
}
END_TEST

START_TEST(test_duplicate_insert) {
    int_table();
    htinsert_i("foobar", 301, table);
    htinsert_i("foobar", 300, table);
    ck_assert_int_eq(htentries(table), 1);
    print_hash_table(print_entry_int, table);
}
END_TEST

START_TEST(test_string_insert) {
    string_table();
    htinsert_s("foo", "shaboozy", table);
    htinsert_s("bar", "kablaam", table);
    ck_assert_int_eq(htentries(table), 2);
    print_hash_table(print_entry_string, table);
}
END_TEST

struct coords { double lat; double lng; char notes[128]; };
struct coords_pair { char *key; struct coords val; };

static void print_coords(void const *coords) {
    struct coords const *x = coords;
    printf("%lf, %lf, %s", x->lat, x->lng, x->notes);
}

static int coords_pair_cmp(void const *a, void const *b) {
    struct coords_pair const *x = a, *y = b;
    return strcmp(x->key, y->key);
}

START_TEST(test_struct_insert) {
    printf("SIZEOF :%ld\n", sizeof (double));
    table = hash_table(sizeof (struct coords));

    struct coords x = { 95.0, 184.3, "x coords" },
                  y = { 2.3, 45.641, "y coords" };

    htinsert("foo", &x, table);
    htinsert("bar", &y, table);

    printf("struct insert:\n");

    struct coords_pair *pairs = htpairs(table);
    qsort(pairs, 2, sizeof (struct coords_pair), coords_pair_cmp);
    ck_assert_str_eq(pairs[0].key, "bar");
    ck_assert_str_eq(pairs[1].key, "foo");
    free(pairs);

    ck_assert_int_eq(htentries(table), 2);
    print_hash_table(print_coords, table);
}
END_TEST


START_TEST(test_contains) {
    int_table();
    htinsert_i("foo", 300, table);
    ck_assert(htcontains("foo", table));
    ck_assert(!htcontains("bar", table));
}
END_TEST

START_TEST(test_lookup) {
    int_table();
    htinsert_i("", 931, table);
    int *x = htlookup("", table);
    ck_assert_int_eq(*x, 931);
    ck_assert(htlookup("foo", table) == NULL);
}
END_TEST

START_TEST(test_random_inserts_and_deletes) {
    int_table();
    char *max_inserts = getenv("MAX_INSERTS");
    int num_inserts = 100;

    if (max_inserts) {
        num_inserts = atoi(max_inserts);
    }

    int key_size = ceil(num_inserts / 5.0);
    char *key = calloc(key_size, sizeof *key);

    for (int t = 0, n = randr(10, num_inserts); t < n; t++) {
        int i;
        for (i = 0; i < randr(1, key_size - 1); i++) {
            do (key[i] = randr(0, 255));
            while (!isprint(key[i]));
        }
        key[i] = '\0';

        htinsert_i(key, i, table);
    }

    free(key);

    printf("random_inserts_and_deletes:\n");
    print_hash_table(print_entry_int, table);

    char **keys = htkeys(table);
    size_t entries = htentries(table);

    int deleted = 0;
    for (int i = 0; i < entries; i++) {
        if (!randr(0, 7)) continue;
        deleted++;
        ck_assert_msg(htdelete(keys[i], table), keys[i]);
    }
    free(keys);

    ck_assert_int_eq(htentries(table), entries - deleted);

    printf("random_inserts_and_deletes:\n");
    print_hash_table(print_entry_int, table);
}
END_TEST

Suite *hash_table_suite()
{
    Suite *s = suite_create("hash_table");

    TCase *tc_core = tcase_create("core");
    TCase *tc_random = tcase_create("random");

    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_checked_fixture(tc_random, setup, teardown);

    tcase_add_test(tc_core, test_basic_insert);
    tcase_add_test(tc_core, test_keys);
    tcase_add_test(tc_core, test_duplicate_insert);
    tcase_add_test(tc_core, test_string_insert);
    tcase_add_test(tc_core, test_struct_insert);
    tcase_add_test(tc_core, test_contains);
    tcase_add_test(tc_core, test_lookup);
    tcase_add_test(tc_random, test_random_inserts_and_deletes);

    suite_add_tcase(s, tc_core);
    suite_add_tcase(s, tc_random);

    return s;
}

