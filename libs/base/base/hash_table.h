#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/array.h"

#define HT_SIZE 17
#define HT_MIN_LOAD 1.5
#define HT_MAX_LOAD 5.0

struct hash_table {
    struct array **buckets;
    int entries;
    int size;
};

union entry {
    void *v;
    int i;
}:

struct hash_entry {
    char const *key;
    union entry const val;
};

struct hash_table hash_table(struct array **buckets, int size);
struct hash_table *init_hash_table(int size);
struct hash_table *htclone(struct hash_table const *table);
struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table);
bool htcontains(char const *key, struct hash_table const *table);
union entry *htlookup(char const *key, struct hash_table const *table);
bool htdelete(char const *key, struct hash_table *table);
int htentries(struct hash_table const *table);
int htsize(struct hash_table const *table);
void htrehash(struct hash_table *table);
void free_hash_table(struct hash_table *table);

#endif // BASE_HASH_TABLE_H_
