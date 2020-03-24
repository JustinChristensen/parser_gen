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

static size_t _valsize(struct hash_table const *table) {
    return table->valsize;
}

static unsigned int _size(struct hash_table const *table) {
    return *table->size;
}

static unsigned int _used(struct hash_table const *table) {
    return table->used;
}

static size_t _entrysize(struct hash_table const *table) {
    return sizeof (struct hash_entry) + _valsize(table);
}

static void setval(void *val, struct hash_entry *entry, struct hash_table const *table) {
    if (val) memcpy(entry->val, val, _valsize(table));
}

static void update_entry(
    struct hash_entry *entry, char const *key, void *val,
    struct hash_table const *table
) {
    if (key != entry->key) free(entry->key);
    entry->key = (char *) key;
    setval(val, entry, table);
}

static struct hash_entry *hash_entry(
    struct hash_entry *next, char const *key, void *val,
    struct hash_table const *table
) {
    struct hash_entry *entry = malloc(_entrysize(table));
    assert(entry != NULL);
    *entry = (struct hash_entry ) { next, (char *) key };
    setval(val, entry, table);
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
    unsigned int used = _used(table);
    return used ? htentries(table) / (double) used : 0;
}

static struct hash_entry **find_bucket(char const *key, struct hash_table const *table) {
    return table->buckets + ind((unsigned char const *) key, _size(table));
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
    unsigned int prev_size = _size(table);

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

static struct hash_table *_hash_table(size_t valsize, unsigned int *size) {
    size = size ? size : (unsigned int *) start_prime;
    struct hash_entry **buckets = allocate_buckets(*size);
    assert(buckets != NULL);

    struct hash_table *table = malloc(sizeof *table);
    assert(table != NULL);

    *table = (struct hash_table) {
        .valsize = valsize,
        .buckets = buckets,
        .size = size,
        .used = 0,
        .entries = 0
    };

    return table;
}

struct hash_table *hash_table(size_t valsize) {
    return _hash_table(valsize, NULL);
}

struct hash_table *htclone(struct hash_table const *table) {
    if (!table) return NULL;
    struct hash_table *new_table = _hash_table(_valsize(table), table->size);

    new_table->entries = htentries(table);
    new_table->size = table->size;

    for (int i = 0; i < _size(table); i++) {
        new_table->buckets[i] = clone_entry(table->buckets[i], table);
    }

    return new_table;
}

void htinsert(char const *key, void *val, struct hash_table *table) {
    assert(table != NULL);
    assert(key != NULL);
    check_load(table);
    key = strdup(key);
    assert(key != NULL);
    return _htinsert(key, val, table);
}

void htinsert_i(char const *key, int val, struct hash_table *table) {
    htinsert(key, &val, table);
}

void htinsert_s(char const *key, char *val, struct hash_table *table) {
    htinsert(key, &val, table);
}

void htinsert_p(char const *key, void *val, struct hash_table *table) {
    htinsert(key, &val, table);
}

void *htlookup(char const *key, struct hash_table const *table) {
    assert(table != NULL);
    assert(key != NULL);

    struct hash_entry **bucket = find_bucket(key, table);
    struct hash_entry *found = NULL;

    if (*bucket && (found = find_entry(key, *bucket))) {
        return found->val;
    }

    return NULL;
}

bool htcontains(char const *key, struct hash_table const *table) {
    assert(table != NULL);
    assert(key != NULL);
    if (htlookup(key, table)) return true;
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

struct hash_iterator hash_iterator(struct hash_table const *table) {
    assert(table != NULL);
    return (struct hash_iterator) { table, 0, NULL };
}

static bool htnextentry(struct hash_entry **out, struct hash_iterator *it) {
    assert(out != NULL);
    assert(it != NULL);
    assert(it->table != NULL);

    struct hash_table const *table = it->table;
    struct hash_entry **buckets = table->buckets;
    unsigned int size = _size(table);

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

void *htnext(char **key, struct hash_iterator *it) {
    struct hash_entry *entry = NULL;
    if (htnextentry(&entry, it)) {
        if (key) entry_key(key, entry);
        return entry->val;
    }

    return NULL;
}

char **htkeys(struct hash_table const *table) {
    char **keys = calloc(htentries(table), sizeof *keys),
         **k = keys;

    struct hash_iterator it = hash_iterator(table);
    struct hash_entry *entry = NULL;
    while (htnextentry(&entry, &it)) {
        entry_key(k, entry);
        k++;
    }

    return keys;
}

void *htvals(struct hash_table const *table) {
    void *vals = calloc(htentries(table), _valsize(table)),
         *v = vals;

    struct hash_iterator it = hash_iterator(table);
    struct hash_entry *entry = NULL;
    while (htnextentry(&entry, &it)) {
        entry_val(v, _valsize(table), entry);
        v += _valsize(table);
    }

    return vals;
}

struct pair { char *key; char val[]; };

static size_t _pairsize(size_t valsize) {
    return sizeof (struct pair) + valsize;
}

void *htpairs(struct hash_table const *table) {
    size_t pairsize = _pairsize(_valsize(table));
    struct pair *pairs = calloc(htentries(table), pairsize);
    void *pp = pairs;

    struct hash_iterator it = hash_iterator(table);
    struct hash_entry *entry = NULL;
    while (htnextentry(&entry, &it)) {
        struct pair *p = pp;
        entry_key(&p->key, entry);
        entry_val(p->val, _valsize(table), entry);
        pp += pairsize;
    }

    return pairs;
}

static int compair(void const *a, void const *b) {
    struct pair const *x = a, *y = b;
    return strcmp(x->key, y->key);
}

void *htsortedpairs(struct hash_table const *table) {
    size_t pairsize = _pairsize(_valsize(table));
    struct pair *pairs = htpairs(table);
    qsort(pairs, htentries(table), pairsize, compair);
    return pairs;
}

void htfrompairs(struct hash_table *table, unsigned int n, void *pairs) {
    size_t pairsize = _pairsize(_valsize(table));

    for (unsigned int i = 0; i < n; i++) {
        struct pair *p = pairs;
        htinsert(p->key, p->val, table);
        pairs += pairsize;
    }
}

unsigned int htentries(struct hash_table const *table) {
    return table->entries;
}

static void print_hash_entry(
    FILE *handle, void (*print_val) (FILE *handle, void const *val),
    struct hash_entry const *entry
) {
    if (!print_val) return;
    fprintf(handle, "(%s, ", entry->key);
    (*print_val)(handle, entry->val);
    fprintf(handle, ")");
}

void print_entry_int(FILE *handle, void const *val) {
    fprintf(handle, "%d", *((int *) val));
}

void print_entry_string(FILE *handle, void const *val) {
    fprintf(handle, "%s", *((char **) val));
}

void print_hash_table(
    FILE *handle, void (*print_val) (FILE *handle, void const *val),
    struct hash_table const *table
) {
    if (!table || !print_val) return;

    print_table_stats(handle, table);

    struct hash_entry **buckets = table->buckets;

    for (int i = 0; i < _size(table); i++) {
        struct hash_entry *entry = buckets[i];

        fprintf(handle, "%d: ", i);

        if (entry) {
            for (; entry; entry = entry->next) {
                print_hash_entry(handle, print_val, entry);
                fprintf(handle, " ");
            }
        } else
            fprintf(handle, "(empty)");

        fprintf(handle, "\n");
    }
}

void print_hash_entries(
    FILE *handle, void (*print_val) (FILE *handle, void const *val),
    struct hash_table const *table
) {
    if (!table || !print_val) return;

    struct hash_iterator it = hash_iterator(table);
    struct hash_entry *entry = NULL;

    while (htnextentry(&entry, &it)) {
        print_hash_entry(handle, print_val, entry);
        fprintf(handle, "\n");
    }
}

void print_table_stats(FILE *handle, struct hash_table const *table) {
    if (!table) return;
    unsigned int used = _used(table), size = _size(table);
    fprintf(handle, "table: %p\nsize: %u\nentries: %u\nused buckets: %u\nload: %lf\nbucket load: %lf\n",
        table, size, htentries(table), used, htload(table), used / (double) size);
}

void free_hash_table(struct hash_table *table) {
    if (!table) return;

    struct hash_entry **buckets = table->buckets;

    for (int i = 0; i < _size(table); i++) {
        for (struct hash_entry *entry = buckets[i], *next = NULL; entry; entry = next) {
            next = entry->next;
            free_hash_entry(entry);
        }
    }

    free(table->buckets);
    table->buckets = NULL;
    free(table);
}
