#ifndef BASE_HASH_TABLE_H_
#define BASE_HASH_TABLE_H_ 1

#include <stdlib.h>
#include <stdbool.h>

#define HT_GROWTH 1.618     // defined, but not used
#define HT_MIN_LOAD 1.5
#define HT_MAX_LOAD 4.0
#define HT_BUCKET_START 1
#define HT_BUCKET_GROWTH 1

union entry {
    void *p;
    void *s;
    int i;
};

struct hash_entry {
    char *key;
    union entry val;
};

struct hash_node {
    struct hash_node *next;
    struct hash_entry entry;
};

struct hash_table {
    struct hash_node **buckets;
    unsigned int *size;
    unsigned int used;
    unsigned int entries;
};

struct table_iterator {
    struct hash_table const *table;
    int i; // bucket
    struct hash_node *node; // entry
};

struct hash_table hash_table(struct hash_node **buckets, unsigned int *size);
struct hash_table *init_hash_table(unsigned int *size);
struct hash_table *htclone(struct hash_table const *table);
char **htkeys(struct hash_table const *table);
struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table);
struct hash_table *htinsert_i(char const *key, int val, struct hash_table *table);
struct hash_table *htinsert_s(char const *key, char *val, struct hash_table *table);
struct hash_table *htinsert_p(char const *key, void *val, struct hash_table *table);
union entry *htlookup(char const *key, struct hash_table const *table);
bool htlookup_i(int *out, char const *key, struct hash_table const *table);
bool htlookup_s(char **out, char const *key, struct hash_table const *table);
bool htlookup_p(void **out, char const *key, struct hash_table const *table);
bool htcontains(char const *key, struct hash_table const *table);
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
