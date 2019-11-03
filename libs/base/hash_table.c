#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "base/array.h"
#include "base/primes.h"
#include "base/hash_table.h"

// http://www.cse.yorku.ca/~oz/hash.html
static unsigned int hash(unsigned char *str) {
    unsigned int hash = 5381;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c;

    return hash;
}

static unsigned int index(char *key, unsigned int size) {
    return hash(key) % size;
}

static struct array new_bucket(unsigned int init_size) {
    size_t entry_size = sizeof (struct hash_entry);
    void *buf = calloc(init_size, entry_size);
    assert(buf != NULL);
    return array(buf, entry_size, init_size, LINEAR, HT_BUCKET_GROWTH);
}

static struct array clone_bucket(struct array *bucket) {
    struct array cloned = new_bucket(asize(bucket));
    void *buf = cloned->buf;
    cloned = *bucket;
    cloned->buf = buf;
    return cloned;
}

static struct array free_bucket(struct array *bucket) {
    free(bucket->buf);
    bucket->buf = NULL;
    return (struct array) { 0 };
}

static struct hash_entry free_hash_entry(struct hash_entry *entry) {
    free(entry->key);
    entry->key = NULL;
    return (struct hash_entry) { 0 };
}

static void htrehash(struct hash_table *table) {
}

static void ensure_memory(struct hash_table *table) {
    if (htload(table) >= HT_MAX_LOAD) htrehash(table);
}

struct hash_table hash_table(struct array *buckets, unsigned int size) {
    return (struct hash_table) {
        .buckets = buckets,
        .entries = 0,
        .size = size
    };
}

struct hash_table *init_hash_table(unsigned int size) {
    size = size > 0 ? size : HT_SIZE;

    struct array *buckets = calloc(sizeof *buckets, size);
    assert(buckets != NULL);

    struct hash_table *table = malloc(sizeof *table);
    assert(table != NULL);

    *table = hash_table(buckets, size);

    return table;
}

struct hash_table *htclone(struct hash_table const *table) {
    if (!table) return NULL;
    struct array *buckets = table->buckets;
    struct hash_table *new_table = init_hash_table(table->size);

    new_table->entries = table->entries;
    new_table->size = table->size;

    for (int i = 0; i < htsize(table); i++) {
        new_table->buckets[i] = clone_bucket(&table->buckets[i]);
    }

    return new_table;
}

void ht_each_entry(void (*fn) (struct hash_entry *entry), struct hash_table const *table) {
    if (!fn) return;

    struct array *buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        struct array *bucket = &buckets[i];

        if (bucket->buf) {
            for (int j = 0; j < asize(bucket); j++) {
                (*fn)(aptr(j, bucket));
            }
        }
    }
}

void ht_each(void (*fn) (union entry val), struct hash_table const *table) {
    if (!fn) return;

    struct array *buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        struct array *bucket = &buckets[i];

        if (bucket->buf) {
            for (int j = 0; j < asize(bucket); j++) {
                (*fn)(aptr(j, bucket)->val);
            }
        }
    }
}

static double htload(struct hash_table const *table) {
    return htentries(table) / htsize(table);
}

static struct array *find_bucket(char const *key, struct array *bucket) {
    return &table->buckets[index(key, table->size)];
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
    assert(key != NULL);

    if (!table) table = init_hash_table(HT_SIZE);
    else        ensure_memory(table);

    struct array *bucket = find_bucket(key, table);

    if (!bucket->buf) {
        *bucket = new_bucket(HT_BUCKET_START);
    }

    char *dupkey = strdup(key);
    assert(dupkey != NULL);
    struct hash_entry new_entry = { dupkey , val };

    struct hash_entry *entry = find_entry(key, bucket);

    if (entry) {
        free_hash_entry(entry);
        // caller needs to worry about maintaining pointers to the
        // entry value (for the void * member of the union) for later freeing
        *entry = new_entry;
    }
    else {
        apush(&new_entry, bucket);
        table->entries++;
    }

    return table;
}

bool htcontains(char const *key, struct hash_table const *table) {
    assert(key != NULL);

    if (!table) return false;

    struct array *bucket = find_bucket(key, table);
    if (bucket && find_entry(key, bucket)) return true;
    return false;
}

union entry *htlookup(char const *key, struct hash_table const *table) {
    assert(key != NULL);

    if (!table) return NULL;

    struct array *bucket = find_bucket(key, table);
    struct hash_entry *entry = NULL;

    if (bucket && (entry = find_entry(key, bucket))) {
        return &entry->val;
    }

    return NULL;
}

bool htdelete(char const *key, struct hash_table *table) {
    assert(key != NULL);

    if (!table) return false;

    struct array *bucket = find_bucket(key, table);
    struct hash_entry *entry = NULL;

    if (bucket && (entry = find_entry(key, bucket))) {
        free_hash_entry(entry);
        adel(entry, bucket);
        table->entries--;

        if (aempty(bucket)) {
            *bucket = free_bucket(bucket);
            // TODO: keep track of the number of buckets?
        }

        return true;
    }

    return false;
}

unsigned int htentries(struct hash_table const *table) {
    return table->entries;
}

unsigned int htsize(struct hash_table const *table) {
    return table->size;
}

void free_hash_table(struct hash_table *table) {
    if (!table) return;
    ht_each_entry(free_hash_entry, table);
    table->buckets = NULL;
    free(table->buckets);
    free(table);
}
