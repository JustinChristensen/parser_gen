#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdio.h>
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

struct hash_iterator {
    struct hash_table const *table;
    int i; // current bucket
    struct hash_entry *entry; // current entry
};

struct hash_table *hash_table(size_t valsize);
void free_hash_table(struct hash_table *table);
struct hash_table *htclone(struct hash_table const *table);
void htinsert(char const *key, void *val, struct hash_table *table);
void htinsert_i(char const *key, int val, struct hash_table *table);
void htinsert_s(char const *key, char *val, struct hash_table *table);
void htinsert_p(char const *key, void *val, struct hash_table *table);
void *htlookup(char const *key, struct hash_table const *table);
bool htcontains(char const *key, struct hash_table const *table);
bool htdelete(char const *key, struct hash_table *table);
struct hash_iterator hash_iterator(struct hash_table const *table);
void *htnext(char **key, struct hash_iterator *it);
char **htkeys(struct hash_table const *table);
void *htvals(struct hash_table const *table);
void *htpairs(struct hash_table const *table);
void *htsortedpairs(struct hash_table const *table);
void htfrompairs(struct hash_table *table, unsigned int n, void *pairs);
unsigned int htentries(struct hash_table const *table);
void htclear(struct hash_table *table);
void print_entry_int(FILE *handle, void const *val);
void print_entry_string(FILE *handle, void const *val);
void print_hash_table(FILE *handle, void (*print_val) (FILE *handle, void const *val), struct hash_table const *table);
void print_hash_entries(FILE *handle, void (*print_val) (FILE *handle, void const *val), struct hash_table const *table);
void print_table_stats(FILE *handle, struct hash_table const *table);

#endif // BASE_HASH_TABLE_H_
