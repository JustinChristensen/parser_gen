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
#include "test_base.h"

struct hash_table *table = NULL;

static void setup() {
}

static void teardown() {
    free_hash_table(table);
    table = NULL;
}

START_TEST(test_basic_insert) {
    char c;
    int i;

    for (c = 'a', i = 3; c < 'h'; c++, i += 2) {
        char key[] = { c, '\0' };
        union entry val = { .i = i };
        table = htinsert(key, val, table);
    }

    for (c = 'a'; c < 'h'; c++) {
        char key[] = { c, '\0' };
        ck_assert(htcontains(key, table));
    }

    print_hash_table(print_hash_int, table);
}
END_TEST

START_TEST(test_keys) {
    union entry v = { .i = 99 };
    table = htinsert("foo", v, table);
    table = htinsert("bar", v, table);
    table = htinsert("baz", v, table);

    char **keys = htkeys(table);

    printf("htkeys:\n");
    unsigned int entries = htentries(table);

    for (int i = 0; i < entries; i++) {
        printf("%s ", keys[i]);
        ck_assert(htdelete(keys[i], table));
    }
    printf("\n");

    free_keys(keys, entries);
}
END_TEST

START_TEST(test_duplicate_insert) {
    union entry val = { .i = 3000 };
    table = htinsert("foobar", val, table);
    table = htinsert("foobar", val, table);
    ck_assert_int_eq(htentries(table), 1);

    print_hash_table(print_hash_int, table);
}
END_TEST

START_TEST(test_random_inserts_and_deletes) {
    char *max_inserts = getenv("MAX_INSERTS");
    int num_inserts = 100;

    if (max_inserts) {
        num_inserts = atoi(max_inserts);
    }

    int key_size = ceil(num_inserts / 5);
    char *key = calloc(key_size, sizeof *key);

    for (int t = 0, n = randr(10, num_inserts); t < n; t++) {
        int i;
        for (i = 0; i < randr(1, key_size - 1); i++) {
            do (key[i] = randr(0, 255));
            while (!isprint(key[i]));
        }
        key[i] = '\0';

        union entry val = { .i = i };
        table = htinsert(key, val, table);
    }

    free(key);

    printf("random_inserts_and_deletes:\n");
    print_hash_table(print_hash_int, table);

    char **keys = htkeys(table);
    size_t entries = htentries(table);

    for (int i = 0; i < entries; i++) {
        if (!randr(0, 7)) continue;
        ck_assert_msg(htdelete(keys[i], table), keys[i]);
    }

    free_keys(keys, entries);

    printf("random_inserts_and_deletes:\n");
    print_hash_table(print_hash_int, table);
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
    tcase_add_test(tc_random, test_random_inserts_and_deletes);

    suite_add_tcase(s, tc_core);
    suite_add_tcase(s, tc_random);

    return s;
}

