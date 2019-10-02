#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    void *buf = calloc(5, 24);
    void *p = buf;
    memcpy(p, "aaaaaaaaaaaaaaaaaaaaaaa", 24);
    p += 24;
    memcpy(p, "bbbbbbbbbbbbbbbbbbbbbbb", 24);
    p += 24;
    memcpy(p, "ccccccccccccccccccccccc", 24);
    p += 24;
    memcpy(p, "ddddddddddddddddddddddd", 24);
    p += 24;
    memcpy(p, "eeeeeeeeeeeeeeeeeeeeeee", 24);
    p = buf;
    printf("%s\n", p + (24 * 0));
    printf("%s\n", p + (24 * 1));
    printf("%s\n", p + (24 * 2));
    printf("%s\n", p + (24 * 3));
    printf("%s\n", p + (24 * 4));
    free(buf);
    return 0;
}
