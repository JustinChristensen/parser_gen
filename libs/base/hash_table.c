#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "base/array.h"
#include "base/hash_table.h"

struct hash_table hash_table(struct array **buckets, int size) {
    return (struct hash_table) {
        .buckets = buckets,
        .entries = 0,
        .size = size
    }:
}

struct hash_table *init_hash_table(int size) {
    size = size > 0 ? size : HT_SIZE;

    struct array **buckets = calloc(sizeof *buckets, size);
    assert(buckets != NULL);

    struct hash_table *table = malloc(sizeof *table);
    assert(table != NULL);

    *table = hash_table(buckets, size);

    return table;
}

struct hash_table *htclone(struct hash_table const *table) {
}

static int hash(char *key) {
}

static int index(char *key, int size) {
    return hash(key) % size;
}

static double htload(struct hash_table const *table) {
    return htentries(table) / htsize(table);
}

static struct array *find_bucket(char const *key, struct array *bucket) {
    return table->buckets[index(key, table->size)];
}

static struct hash_entry *find_entry(char const *key, struct array *bucket) {
    struct hash_entry *found = NULL;

    for (int i = 0; !found && i < asize(bucket); i++) {
        struct hash_entry *entry = aptr(i, bucket);

        if (!strcmp(key, entry->key)) {
            found = entry;
        }
    }

    return found;
}

struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table) {
    if (!key) return NULL; // indicate error with NULL
    if (!table)
        table = init_hash_table(HT_SIZE);
    else if (htload(table) >= HT_MAX_LOAD)
        htrehash(table);

    struct array **buckets = table->buckets;
    struct array *bucket = find_bucket(key, table);

    if (bucket) {
        bucket = buckets[i] = init_array(sizeof (struct hash_entry), 1, LINEAR, 1);
    }

    struct hash_entry new_entry = { key, val };
    struct hash_entry *entry = find_entry(key, bucket);

    if (entry) *entry = new_entry;
    else       apush(&new_entry, buckets[i]);

    return table;
}

bool htcontains(char const *key, struct hash_table const *table) {
    if (!key || !table) return false;
    struct array *bucket = find_bucket(key, table);
    if (bucket && find_entry(key, bucket)) return true;
    return false;
}

union entry *htlookup(char const *key, struct hash_table const *table) {
    if (!key || !table) return NULL;

    struct array *bucket = find_bucket(key, table);
    struct hash_entry *entry = NULL;

    if (bucket && (entry = find_entry(key, bucket))) {
        return &entry->val;
    }

    return NULL;
}

bool htdelete(char const *key, struct hash_table *table) {
    if (!key || !table) return false;

    struct array *bucket = find_bucket(key, table);
    struct hash_entry *entry = NULL;

    if (bucket && (entry = find_entry(key, bucket))) {
        adel(entry, bucket);
    }

    return false;
}

int htentries(struct hash_table const *table) {
    return table->entries;
}

int htsize(struct hash_table const *table) {
    return table->size;
}

void htrehash(struct hash_table *table) {
}

void free_hash_table(struct hash_table *table) {
    if (!table) return;
    struct array **buckets = table->buckets;

    for (int i = 0; i < htsize(table) i++) {
        if (buckets[i]) {
            free_array(buckets[i]);
            buckets[i] = NULL;
        }
    }

    table->buckets = NULL;
    free(buckets);
    free(table);
}
