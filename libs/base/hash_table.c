#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
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

static struct hash_entry hash_entry(char const *key, union entry val) {
    char *dupkey = strdup(key);
    assert(dupkey != NULL);
    return (struct hash_entry) { dupkey, val };
}

static struct hash_node *init_hash_node(struct hash_node *next, struct hash_entry entry) {
    struct hash_node *node = malloc(sizeof *node);
    assert(node != NULL);
    *node = (struct hash_node) { next, entry };
    return node;
}

static struct hash_node *clone_node(struct hash_node *node) {
    if (!node) return NULL;
    struct hash_entry entry = node->entry;
    return init_hash_node(clone_node(node->next), hash_entry(entry.key, entry.val));
}

static struct hash_entry free_hash_entry(struct hash_entry entry) {
    free(entry.key);
    entry.key = NULL;
    return entry;
}

static void free_hash_node(struct hash_node *node) {
    if (!node) return;
    node->entry = free_hash_entry(node->entry);
    free(node);
}

static struct hash_node **allocate_buckets(unsigned int size) {
    struct hash_node **buckets = calloc(sizeof *buckets, size);
    assert(buckets != NULL);
    return buckets;
}

static double htload(struct hash_table const *table) {
    return htentries(table) / (double) htused(table);
}

static struct hash_node **find_bucket(char const *key, struct hash_table const *table) {
    return table->buckets + ind((unsigned char const *) key, *table->size);
}

static struct hash_node *find_node(char const *key, struct hash_node *bucket) {
    for (struct hash_node *node = bucket; node; node = node->next) {
        struct hash_entry entry = node->entry;
        if (!strcmp(key, entry.key)) return node;
    }

    return NULL;
}

static struct hash_node *delete_node(char const *key, struct hash_node *node) {
    if (!node) return NULL;
    struct hash_node *next = node;
    struct hash_entry entry = node->entry;

    if (!strcmp(key, entry.key)) {
        next = node->next;
        free_hash_node(node);
    } else {
        node->next = delete_node(key, node->next);
    }

    return next;
}

// without the load checking
static struct hash_table *_htinsert(struct hash_entry new_entry, struct hash_table *table) {
    struct hash_node **bucket = find_bucket(new_entry.key, table);
    struct hash_node *found = find_node(new_entry.key, *bucket);

    if (found) {
        if (found->entry.key != new_entry.key) {
            free(found->entry.key);
        }
        // caller needs to worry about maintaining pointers to the
        // entry value (for the void * member of the union) for later freeing
        found->entry = new_entry;
    } else {
        if (*bucket == NULL) table->used++;
        *bucket = init_hash_node(*bucket, new_entry);
        table->entries++;
    }

    return table;
}

static void rehash(struct hash_table *table, unsigned int *size) {
    struct hash_node **buckets = table->buckets;
    unsigned int prev_size = htsize(table);

    table->buckets = allocate_buckets(*size);
    table->used = 0;
    table->entries = 0;
    table->size = size;

    for (int i = 0; i < prev_size; i++) {
        for (struct hash_node *node = buckets[i], *next = NULL; node; node = next) {
            next = node->next;
            _htinsert(node->entry, table);
            free(node);
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

struct hash_table hash_table(struct hash_node **buckets, unsigned int *size) {
    return (struct hash_table) {
        .buckets = buckets,
        .size = size,
        .used = 0,
        .entries = 0
    };
}

struct hash_table *init_hash_table(unsigned int *size) {
    size = size ? size : (unsigned int *) start_prime;
    struct hash_node **buckets = allocate_buckets(*size);
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
        new_table->buckets[i] = clone_node(table->buckets[i]);
    }

    return new_table;
}

char **htkeys(struct hash_table const *table) {
    char **keys = calloc(htentries(table), sizeof *keys);

    struct table_iterator it = table_iterator(table);
    struct hash_entry *entry;
    char **k = keys;
    while (htnextentry(&entry, &it)) {
        *k++ = entry->key;
    }

    return keys;
}

struct hash_table *htinsert(char const *key, union entry const val, struct hash_table *table) {
    assert(key != NULL);

    if (!table) table = init_hash_table(NULL);
    else        check_load(table);

    return _htinsert(hash_entry(key, val), table);
}

struct hash_table *htinsert_i(char const *key, int val, struct hash_table *table) {
    return htinsert(key, (union entry) { .i = val }, table);
}

struct hash_table *htinsert_s(char const *key, char *val, struct hash_table *table) {
    return htinsert(key, (union entry) { .s = val }, table);
}

struct hash_table *htinsert_p(char const *key, void *val, struct hash_table *table) {
    return htinsert(key, (union entry) { .p = val }, table);
}

union entry *htlookup(char const *key, struct hash_table const *table) {
    assert(key != NULL);

    if (!table) return NULL;

    struct hash_node **bucket = find_bucket(key, table);
    struct hash_node *found = NULL;

    if (*bucket && (found = find_node(key, *bucket))) {
        return &found->entry.val;
    }

    return NULL;
}

bool htlookup_i(int *out, char const *key, struct hash_table const *table) {
    union entry *v = htlookup(key, table);
    if (!v) return false;
    *out = v->i;
    return true;
}

bool htlookup_s(char **out, char const *key, struct hash_table const *table) {
    union entry *v = htlookup(key, table);
    if (!v) return false;
    *out = v->s;
    return true;
}

bool htlookup_p(void **out, char const *key, struct hash_table const *table) {
    union entry *v = htlookup(key, table);
    if (!v) return false;
    *out = v->p;
    return true;
}

bool htcontains(char const *key, struct hash_table const *table) {
    assert(key != NULL);
    if (htlookup(key, table)) return true;
    return false;
}

bool htdelete(char const *key, struct hash_table *table) {
    assert(table != NULL && key != NULL);
    struct hash_node **bucket = find_bucket(key, table);

    if (*bucket) {
        *bucket = delete_node(key, *bucket);
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

bool htnextentry(struct hash_entry **out, struct table_iterator *it) {
    assert(out != NULL && it != NULL && it->table != NULL);

    struct hash_table const *table = it->table;
    struct hash_node **buckets = table->buckets;
    unsigned int size = htsize(table);

    while (true) {
        struct hash_node *node = it->node;

        if (node) {
            *out = &node->entry;
            it->node = node->next;
            return true;
        }

        if (it->i < size) {
            it->node = buckets[it->i++];
            continue;
        }

        break;
    }

    *out = NULL;
    it->i = 0;
    it->node = NULL;

    return false;
}

bool htnext(union entry **out, struct table_iterator *it) {
    struct hash_entry *entry;

    if (htnextentry(&entry, it)) {
        *out = &entry->val;
    }

    return false;
}

struct hash_table *from_entry_list(struct hash_entry *entries, size_t n) {
    struct hash_table *table = NULL;

    for (int i = 0; i < n; i++) {
        table = htinsert(entries[i].key, entries[i].val, table);
    }

    return table;
}

struct hash_entry *to_entry_list(struct hash_table const *table) {
    struct table_iterator it = table_iterator(table);
    struct hash_entry *entries = calloc(htentries(table), sizeof *entries);

    struct hash_entry *e = entries;
    struct hash_entry *entry;

    while (htnextentry(&entry, &it)) {
        *e++ = (struct hash_entry) { strdup(entry->key), entry->val };
    }

    return entries;
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

void print_entry_int(union entry val) {
    printf("%d", val.i);
}

void print_entry_string(union entry val) {
    printf("%s", val.s);
}

static void print_hash_entry(void (*print_val) (union entry val), struct hash_entry const entry) {
    if (!print_val) return;
    printf("(%s, ", entry.key);
    (*print_val)(entry.val);
    printf(")");
}

void print_hash_table(void (*print_val) (union entry val), struct hash_table const *table) {
    if (!table || !print_val) return;

    print_table_stats(table);

    struct hash_node **buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        struct hash_node *node = buckets[i];

        printf("%d: ", i);

        if (node) {
            for (; node; node = node->next) {
                print_hash_entry(print_val, node->entry);
                printf(" ");
            }
        } else {
            printf("(empty)");
        }

        printf("\n");
    }
}

void print_hash_entries(void (*print_val) (union entry val), struct hash_table const *table) {
    if (!table || !print_val) return;

    struct table_iterator it = table_iterator(table);
    struct hash_entry *entry;

    while (htnextentry(&entry, &it)) {
        print_hash_entry(print_val, *entry);
        printf("\n");
    }
}

void print_table_stats(struct hash_table const *table) {
    if (!table) return;

    printf("table: %p\nsize: %u\nentries: %u\nused: %u\nload factor: %lf\n",
        table, htsize(table), htentries(table), htused(table), htload(table));
}

void free_hash_table(struct hash_table *table) {
    if (!table) return;

    struct hash_node **buckets = table->buckets;

    for (int i = 0; i < htsize(table); i++) {
        for (struct hash_node *node = buckets[i], *next = NULL; node; node = next) {
            next = node->next;
            free_hash_node(node);
        }
    }

    free(table->buckets);
    table->buckets = NULL;
    free(table);
}
