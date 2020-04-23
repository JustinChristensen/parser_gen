#ifndef TEST_BASE_H_
#define TEST_BASE_H_ 1

#include <check.h>

#define RUN_DIAGNOSTICS "RUN_DIAGNOSTICS"

Suite *intset_suite();
Suite *array_suite();
Suite *hash_table_suite();
Suite *rbtree_suite();
Suite *bitset_suite();

#endif // TEST_BASE_H_
