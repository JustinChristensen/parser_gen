#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>

enum opt_keys {
    BUFFY,
    FLUORIDE,
    DAGGERSET
};

int main(int argc, char *argv[]) {
    int flag = -1;
    int ret = -1;

    printf("argc: %d\n", argc);

    struct option longopts[] = {
        { "buffy",      no_argument,            &flag,      BUFFY },
        { "fluoride",   required_argument,      &flag,      FLUORIDE },
        { "daggerset",  no_argument,            &flag,      DAGGERSET },
        { NULL,         0,                      &flag,      0 }
    };

    ret = getopt_long(argc, argv, "bf:", longopts, NULL);
    printf("ret: %d\n", ret);
    printf("flag: %d\n", flag);
    printf("val: %s\n", optarg);

    flag = ret = -1;
    ret = getopt_long(argc, argv, "bf:", longopts, NULL);
    printf("ret: %d\n", ret);
    printf("flag: %d\n", flag);
    printf("val: %s\n", optarg);

    return 0;
}
