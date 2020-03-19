#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdbool.h>

#define HT_GROWTH 1.618     // defined, but not used
#define HT_MIN_LOAD 1.5
#define HT_MAX_LOAD 4.0
#define HT_BUCKET_START 1
#define HT_BUCKET_GROWTH 1

struct hash_entry {
    struct hash_entry *next;
    char *key;
    char val[];
};

struct hash_table {
    size_t valsize;
    struct hash_entry **buckets;
    unsigned int *size;
    unsigned int used;
    unsigned int entries;
};

struct table_iterator {
    struct hash_table const *table;
    int i; // current bucket
    struct hash_entry *entry; // current entry
};

struct hash_table *init_hash_table(unsigned int *size, size_t valsize);
void free_hash_table(struct hash_table *table);
struct hash_table *htclone(struct hash_table const *table);
void htinsert(char const *key, void *val, struct hash_table *table);
bool htlookup(void *out, char const *key, struct hash_table const *table);
bool htcontains(char const *key, struct hash_table const *table);
bool htdelete(char const *key, struct hash_table *table);
struct table_iterator table_iterator(struct hash_table const *table);
bool htnext(char **key, void *out, struct table_iterator *it);
char **htkeys(struct hash_table const *table);
void *htvals(struct hash_table const *table);
unsigned int htsize(struct hash_table const *table);
unsigned int htentries(struct hash_table const *table);
unsigned int htused(struct hash_table const *table);
void print_hash_table(void (*print_val) (void const *val), struct hash_table const *table);
void print_hash_entries(void (*print_val) (void const *val), struct hash_table const *table);
void print_table_stats(struct hash_table const *table);

#endif // BASE_HASH_TABLE_H_
