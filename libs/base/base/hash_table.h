#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/array.h"

#define HT_SIZE 7
#define HT_GROWTH 1.618
#define HT_MIN_LOAD 1.5
#define HT_MAX_LOAD 5.0
#define HT_BUCKET_START 1
#define HT_BUCKET_GROWTH 1

struct hash_table {
    struct array *buckets;
    unsigned int entries;
    unsigned int size;
};

union entry {
    void *v;
    int i;
}:

struct hash_entry {
    char const *key;
    union entry const val;
};

struct hash_table hash_table(struct array *buckets, unsigned int size);
struct hash_table *init_hash_table(unsigned int size);
void ht_each_entry(void (*fn) (struct hash_entry *entry), struct hash_table const *table);
void ht_each(void (*fn) (union entry val), struct hash_table const *table);
struct hash_table *htclone(struct hash_table const *table);
struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table);
bool htcontains(char const *key, struct hash_table const *table);
union entry *htlookup(char const *key, struct hash_table const *table);
bool htdelete(char const *key, struct hash_table *table);
unsigned int htentries(struct hash_table const *table);
unsigned int htsize(struct hash_table const *table);
void free_hash_table(struct hash_table *table);

#endif // BASE_HASH_TABLE_H_
