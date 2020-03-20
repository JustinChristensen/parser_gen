#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct hash_entry {
    char *key;
    size_t vsize;
    char val[];
};

struct coords {
    double x;
    double y;
    char buf[BUFSIZ];
};

struct hash_entry *coords_entry(char *key, struct coords val) {
    size_t vsize = sizeof val;
    struct hash_entry *entry = malloc(sizeof *entry + vsize);
    assert(entry != NULL);

    entry->key = key;
    entry->vsize = vsize;
    memcpy(entry->val, &val, vsize);

    return entry;
}

char *entry_key(struct hash_entry *entry) {
    return entry->key;
}

void entry_val(void *val, struct hash_entry *entry) {
    memcpy(val, entry->val, entry->vsize);
}

void *entry_val_ptr(struct hash_entry *entry) {
    return entry->val;
}

void print_coords_entry(struct hash_entry *entry) {
    struct coords val;
    entry_val(&val, entry);
    printf("%s: (%lf, %lf) %s\n", entry_key(entry), val.x, val.y, val.buf);
}

void print_coords_entry_ptr(struct hash_entry *entry) {
    struct coords *val = entry_val_ptr(entry);;
    printf("%s: (%lf, %lf) %s\n", entry_key(entry), val->x, val->y, val->buf);
}

int main(int argc, char *argv[]) {
    printf("sizeof %ld\n", sizeof (struct hash_entry));

    struct hash_entry
        *x = coords_entry("foo", (struct coords) { 35.6, 94.1, "teh quick brown fox" }),
        *y = coords_entry("bar", (struct coords) { 96.3, 2.0, "uber leet" });

    print_coords_entry(x);
    print_coords_entry_ptr(y);

    free(x);
    free(y);

    return 0;
}
