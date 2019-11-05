#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdbool.h>
#include "base/array.h"

#define HT_GROWTH 1.618     // defined, but not used
#define HT_MIN_LOAD 1.5
#define HT_MAX_LOAD 4.0
#define HT_BUCKET_START 1
#define HT_BUCKET_GROWTH 1

struct hash_table {
    struct array *buckets;
    unsigned int *size;
    unsigned int used;
    unsigned int entries;
};

union entry {
    void *v;
    int i;
};

struct hash_entry {
    char *key;
    union entry val;
};

struct table_iterator {
    struct hash_table const *table;
    int i; // bucket
    int j; // entry
};

struct hash_table hash_table(struct array *buckets, unsigned int *size);
struct hash_table *init_hash_table(unsigned int *size);
void ht_each_entry(void (*fn) (struct hash_entry *entry, void *state), void *state, struct hash_table const *table);
void ht_each(void (*fn) (union entry *val, void *state), void *state, struct hash_table const *table);
// bool hash_table_eq(bool (*valeq) (union entry const *a, union entry const *b), struct hash_table const *a, struct hash_table const *b);
// bool hash_int_eq(union entry const *a, union entry const *b);
struct hash_table *htclone(struct hash_table const *table);
char **htkeys(struct hash_table const *table);
void free_keys(char **keys, size_t n);
struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table);
bool htcontains(char const *key, struct hash_table const *table);
union entry *htlookup(char const *key, struct hash_table const *table);
bool htdelete(char const *key, struct hash_table *table);
struct table_iterator table_iterator(struct hash_table const *table);
bool htnextentry(struct hash_entry **out, struct table_iterator *it);
bool htnext(union entry **out, struct table_iterator *it);
struct hash_table *from_entry_list(struct hash_entry *entries, size_t n);
struct hash_entry *to_entry_list(struct hash_table const *table);
unsigned int htsize(struct hash_table const *table);
unsigned int htentries(struct hash_table const *table);
unsigned int htused(struct hash_table const *table);
void print_hash_int(union entry val);
void print_hash_table(void (*print_val) (union entry val), struct hash_table const *table);
void print_hash_entries(void (*print_val) (union entry val), struct hash_table const *table);
void print_table_stats(struct hash_table const *table);
void free_hash_table(struct hash_table *table);

#endif // BASE_HASH_TABLE_H_
