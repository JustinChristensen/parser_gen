#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/array.h"

struct hash_table {
    struct array **buckets;
};

struct hash_entry {
    char const *key;
    void const *entry;
};

struct hash_table hash_table();
struct hash_table *init_hash_table(struct array **buckets);
int hash(char *key);
int index(char *key, int array_size);
void *htlookup(char const *key, struct hash_table const *table);
void htinsert(char const *key, void const *val, struct hash_table *table);
void htdelete(char const *key, struct hash_table *table);
double htloadfact(struct hash_table const *table);
void free_hash_table(struct hash_table *table);

#endif // BASE_HASH_TABLE_H_
