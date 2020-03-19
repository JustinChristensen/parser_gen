#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "base/hash_table.h"
#include "base/primes.h"
#include "base/string.h"

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

static void update_entry(
    struct hash_entry *entry, char const *key, void *val,
    struct hash_table const *table
) {
    if (key != entry->key) {
        free(entry->key);
        key = strdup(key);
        assert(key != NULL);
    }

    entry->key = (char *) key;
    memcpy(entry->val, val, table->valsize);
}

static struct hash_entry *hash_entry(
    struct hash_entry *next, char const *key, void *val,
    struct hash_table const *table
) {
    struct hash_entry *entry = malloc(sizeof *entry + table->valsize);
    assert(entry != NULL);
    entry->next = next;
    update_entry(entry, key, val, table);
    return entry;
}

static void entry_key(char **key, struct hash_entry *entry)
    { *key = entry->key; }
static void entry_val(void *out, size_t valsize, struct hash_entry *entry)
    { memcpy(out, entry->val, valsize); }

static struct hash_entry *clone_entry(struct hash_entry *entry, struct hash_table const *table) {
    if (!entry) return NULL;
    return hash_entry(clone_entry(entry->next, table), entry->key, entry->val, table);
}

static void free_hash_entry(struct hash_entry *entry) {
    if (!entry) return;
    free(entry->key);
    free(entry);
}

static struct hash_entry **allocate_buckets(unsigned int size) {
    struct hash_entry **buckets = calloc(sizeof *buckets, size);
    assert(buckets != NULL);
    return buckets;
}

static double htload(struct hash_table const *table) {
    return htentries(table) / (double) htused(table);
}

static struct hash_entry **find_bucket(char const *key, struct hash_table const *table) {
    return table->buckets + ind((unsigned char const *) key, *table->size);
}

static struct hash_entry *find_entry(char const *key, struct hash_entry *entry) {
    if (!entry) return NULL;

    if (streq(key, entry->key)) return entry;
    else return find_entry(key, entry->next);
}

static struct hash_entry *delete_entry(char const *key, struct hash_entry *entry) {
    if (!entry) return NULL;

    struct hash_entry *next = entry;

    if (streq(key, entry->key)) {
        next = entry->next;
        free_hash_entry(entry);
    } else {
        entry->next = delete_entry(key, entry->next);
    }

    return next;
}

// without the load checking
static void _htinsert(char const *key, void *val, struct hash_table *table) {
    struct hash_entry **bucket = find_bucket(key, table);
    struct hash_entry *found = find_entry(key, *bucket);

    if (found) update_entry(found, key, val, table);
    else {
        if (*bucket == NULL) table->used++;
        *bucket = hash_entry(*bucket, key, val, table);
        table->entries++;
    }
}

static void rehash(struct hash_table *table, unsigned int *size) {
    struct hash_entry **buckets = table->buckets;
    unsigned int prev_size = htsize(table);

    table->buckets = allocate_buckets(*size);
    table->used = 0;
    table->entries = 0;
    table->size = size;

    for (int i = 0; i < prev_size; i++) {
        for (struct hash_entry *entry = buckets[i], *next = NULL; entry; entry = next) {
            next = entry->next;
            _htinsert(entry->key, entry->val, table);
            free(entry);
        }

        buckets[i] = NULL;
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

static struct hash_table hash_table(struct hash_entry **buckets, unsigned int *size, size_t valsize) {
    return (struct hash_table) {
        .valsize = valsize,
        .buckets = buckets,
        .size = size,
        .used = 0,
        .entries = 0
    };
}

struct hash_table *init_hash_table(unsigned int *size, size_t valsize) {
    assert(valsize > 0);

    size = size ? size : (unsigned int *) start_prime;
    struct hash_entry **buckets = allocate_buckets(*size);
    assert(buckets != NULL);

    struct hash_table *table = malloc(sizeof *table);
    assert(table != NULL);

    *table = hash_table(buckets, size, valsize);

    return table;
}

struct hash_table *htclone(struct hash_table const *table) {
    if (!table) return NULL;
    struct hash_table *new_table = init_hash_table(table->size, table->valsize);

    new_table->entries = table->entries;
    new_table->size = table->size;

    for (int i = 0; i < htsize(table); i++) {
        new_table->buckets[i] = clone_entry(table->buckets[i], table);
    }

    return new_table;
}

void htinsert(char const *key, void *val, struct hash_table *table) {
    assert(table != NULL);
    assert(key != NULL);
    assert(val != NULL);
    check_load(table);
    return _htinsert(key, val, table);
}

bool htlookup(void *out, char const *key, struct hash_table const *table) {
    assert(table != NULL);
    assert(key != NULL);

    struct hash_entry **bucket = find_bucket(key, table);
    struct hash_entry *found = NULL;

    if (*bucket && (found = find_entry(key, *bucket))) {
        if (out) entry_val(out, table->valsize, found);
        return true;
    }

    return false;
}

bool htcontains(char const *key, struct hash_table const *table) {
    assert(table != NULL);
    assert(key != NULL);
    if (htlookup(NULL, key, table)) return true;
    return false;
}

bool htdelete(char const *key, struct hash_table *table) {
    assert(table != NULL);
    assert(key != NULL);
    struct hash_entry **bucket = find_bucket(key, table);

    if (*bucket) {
        *bucket = delete_entry(key, *bucket);
        table->entries--;
        if (*bucket == NULL) table->used--;
        check_load(table);
        return true;
    }

    return false;
}

struct table_iterator table_iterator(struct hash_table const *table) {
    assert(table != NULL);
    return (struct table_iterator) { table, 0, NULL };
}

static bool htnextentry(struct hash_entry **out, struct table_iterator *it) {
    assert(out != NULL);
    assert(it != NULL);
    assert(it->table != NULL);

    struct hash_table const *table = it->table;
    struct hash_entry **buckets = table->buckets;
    unsigned int size = htsize(table);

    while (true) {
        struct hash_entry *entry = it->entry;

        if (entry) {
            *out = entry;
            it->entry = entry->next;
            return true;
        }

        if (it->i < size) {
            it->entry = buckets[it->i++];
            continue;
        }

        break;
    }

    *out = NULL;
    it->i = 0;
    it->entry = NULL;

    return false;
}

bool htnext(char **key, void *out, struct table_iterator *it) {
    assert(key || out);
    struct hash_table const *table = it->table;
    struct hash_entry *entry = NULL;

    if (htnextentry(&entry, it)) {
        if (key) entry_key(key, entry);
        if (out) entry_val(out, table->valsize, entry);
        return true;
    }

    return false;
}

char **htkeys(struct hash_table const *table) {
    char **keys = calloc(htentries(table), sizeof *keys);

    struct table_iterator it = table_iterator(table);
    struct hash_entry *entry = NULL;
    char **k = keys;
    while (htnextentry(&entry, &it)) {
        entry_key(k, entry);
        k++;
    }

    return keys;
}

void *htvals(struct hash_table const *table) {
    void *vals = calloc(htentries(table), table->valsize);

    struct table_iterator it = table_iterator(table);
    struct hash_entry *entry = NULL;
    void *v = vals;
    while (htnextentry(&entry, &it)) {
        entry_val(v, table->valsize, entry);
        v += table->valsize;
    }

    return vals;
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

static void print_hash_entry(void (*print_val) (void const *val), struct hash_entry const *entry) {
    if (!print_val) return;
    printf("(%s, ", entry->key);
    (*print_val)(entry->val);
    printf(")");
}

void print_hash_table(void (*print_val) (void const *val), struct hash_table const *table) {
    if (!table || !print_val) return;

    print_table_stats(table);

    struct hash_entry **buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        struct hash_entry *entry = buckets[i];

        printf("%d: ", i);

        if (entry) {
            for (; entry; entry = entry->next) {
                print_hash_entry(print_val, entry);
                printf(" ");
            }
        } else {
            printf("(empty)");
        }

        printf("\n");
    }
}

void print_hash_entries(void (*print_val) (void const *val), struct hash_table const *table) {
    if (!table || !print_val) return;

    struct table_iterator it = table_iterator(table);
    struct hash_entry *entry = NULL;

    while (htnextentry(&entry, &it)) {
        print_hash_entry(print_val, entry);
        printf("\n");
    }
}

void print_table_stats(struct hash_table const *table) {
    if (!table) return;
    unsigned int used = htused(table), size = htsize(table);
    printf("table: %p\nsize: %u\nentries: %u\nused buckets: %u\nload: %lf\nbucket load: %lf\n",
        table, size, htentries(table), used, htload(table), used / (double) size);
}

void free_hash_table(struct hash_table *table) {
    if (!table) return;

    struct hash_entry **buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        for (struct hash_entry *entry = buckets[i], *next = NULL; entry; entry = next) {
            next = entry->next;
            free_hash_entry(entry);
        }
    }

    free(table->buckets);
    table->buckets = NULL;
    free(table);
}
