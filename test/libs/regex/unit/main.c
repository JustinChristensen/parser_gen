#include <stdlib.h>
#include <check.h>
#include <base/args.h>
#include "suites.h"

enum command_key {
    ALL
};

struct args {
    enum command_key cmd;
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END);
    args->cmd = cmd;
    args->pos = argv(context);
    args->posc = argc(context);
}

int main(int argc, char *argv[])
{
    struct args args = {
        .cmd = ALL,
        .posc = 0,
        .pos = NULL,
    };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        ALL,
        NULL,
        ARGS {
            help_and_version_args,
            END_ARGS
        },
        NULL,
        "Run the test suite"
    });

    int number_failed;

    SRunner *sr = srunner_create(NULL);
    srunner_add_suite(sr, nfa_suite());
    srunner_run_all(sr, CK_ENV);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return number_failed == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
