#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "base/array.h"
#include "base/primes.h"
#include "base/hash_table.h"

// http://www.cse.yorku.ca/~oz/hash.html
static unsigned int hash(unsigned char const *str) {
    unsigned int hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;

    return hash;
}

static unsigned int ind(unsigned char const *key, unsigned int size) {
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
    void *buf = cloned.buf;
    cloned = *bucket; // copy over the bucket state
    cloned.buf = buf;
    return cloned;
}

static struct array free_bucket(struct array *bucket) {
    free(bucket->buf);
    bucket->buf = NULL;
    return (struct array) { 0 };
}

void free_hash_entry(struct hash_entry *entry) {
    free((void *) entry->key);
    entry->key = NULL;
}

static struct array *allocate_buckets(unsigned int size) {
    struct array *buckets = calloc(sizeof *buckets, size);
    assert(buckets != NULL);
    return buckets;
}

static double htload(struct hash_table const *table) {
    return htentries(table) / htused(table);
}

static struct array *find_bucket(char const *key, struct hash_table const *table) {
    return &table->buckets[ind((unsigned char const *) key, *table->size)];
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

// without the load checking
static struct hash_table *_htinsert(char const *key, union entry const val, struct hash_table *table) {
    struct array *bucket = find_bucket(key, table);

    if (!bucket->buf) {
        *bucket = new_bucket(HT_BUCKET_START);
        table->used++;
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

static void rehash(struct hash_table *table, unsigned int *size) {
    struct array *buckets = table->buckets;
    unsigned int prev_size = htsize(table);

    table->buckets = allocate_buckets(*size);
    table->used = 0;
    table->entries = 0;
    table->size = size;

    for (int i = 0; i < prev_size; i++) {
        struct array bucket = buckets[i];

        if (bucket.buf) {
            while (!aempty(&bucket)) {
                struct hash_entry entry;
                apop(&entry, &bucket);
                _htinsert(entry.key, entry.val, table);
                free_hash_entry(&entry);
            }

            buckets[i] = free_bucket(&bucket);
        }
    }

    free(buckets);
}

static bool should_grow(struct hash_table *table) {
    return table->size < end_prime && htload(table) >= HT_MAX_LOAD;
}

static bool should_shrink(struct hash_table *table) {
    return table->size > start_prime && htload(table) < HT_MIN_LOAD;
}

static void check_load(struct hash_table *table) {
    if (should_grow(table))         rehash(table, table->size + 1);
    else if (should_shrink(table))  rehash(table, table->size - 1);
}

struct hash_table hash_table(struct array *buckets, unsigned int *size) {
    return (struct hash_table) {
        .buckets = buckets,
        .size = size,
        .used = 0,
        .entries = 0
    };
}

struct hash_table *init_hash_table(unsigned int *size) {
    size = size ? size : (unsigned int *) start_prime;
    struct array *buckets = allocate_buckets(*size);
    assert(buckets != NULL);

    struct hash_table *table = malloc(sizeof *table);
    assert(table != NULL);

    *table = hash_table(buckets, size);

    return table;
}

struct hash_table *htclone(struct hash_table const *table) {
    if (!table) return NULL;
    struct hash_table *new_table = init_hash_table(table->size);

    new_table->entries = table->entries;
    new_table->size = table->size;

    for (int i = 0; i < htsize(table); i++) {
        new_table->buckets[i] = clone_bucket(&table->buckets[i]);
    }

    return new_table;
}

void ht_each_entry(void (*fn) (struct hash_entry *entry, void *state), void *state, struct hash_table const *table) {
    if (!fn) return;

    struct array *buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        struct array *bucket = &buckets[i];

        if (bucket->buf) {
            for (int j = 0; j < asize(bucket); j++) {
                (*fn)(aptr(j, bucket), state);
            }
        }
    }
}

void ht_each(void (*fn) (union entry val, void *state), void *state, struct hash_table const *table) {
    if (!fn) return;

    struct array *buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        struct array *bucket = &buckets[i];

        if (bucket->buf) {
            for (int j = 0; j < asize(bucket); j++) {
                struct hash_entry *entry = aptr(j, bucket);
                (*fn)(entry->val, state);
            }
        }
    }
}

struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table) {
    assert(key != NULL);

    if (!table) table = init_hash_table(NULL);
    else        check_load(table);

    return _htinsert(key, val, table);
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
    assert(table != NULL && key != NULL);

    struct array *bucket = find_bucket(key, table);
    struct hash_entry *entry = NULL;

    if (bucket && (entry = find_entry(key, bucket))) {
        free_hash_entry(entry);
        adel(entry, bucket);
        table->entries--;

        if (aempty(bucket)) {
            *bucket = free_bucket(bucket);
            table->used--;
        }

        check_load(table);

        return true;
    }

    return false;
}

unsigned int htsize(struct hash_table const *table) {
    return *table->size;
}

unsigned int htentries(struct hash_table const *table) {
    return table->entries;
}

unsigned int htused(struct hash_table const *table) {
    return table->used;
}

static void print_entry(struct hash_entry *entry, void *state) {
    void (**printers) (union entry val) = state;
    void (*print_val) (union entry val) = printers[0];

    printf("%s: ", entry->key);
    (*print_val)(entry->val);
    printf("\n");
}

void print_hash_int(union entry val) {
    printf("%d", val.i);
}

void print_hash_table(void (*print_val) (union entry val), struct hash_table const *table) {
    void (*printers[]) (union entry val) = { print_val };
    ht_each_entry(print_entry, printers, table);
}

static void _free_hash_entry(struct hash_entry *entry, void *_) {
    free_hash_entry(entry);
}

void free_hash_table(struct hash_table *table) {
    if (!table) return;
    ht_each_entry(_free_hash_entry, NULL, table);
    table->buckets = NULL;
    free(table->buckets);
    free(table);
}
