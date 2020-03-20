#include <stdlib.h>
#include <check.h>
#include <base/args.h>
#include "suites.h"
#include "btree_dot.h"

enum command_key {
    ALL,
    BTREE
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
        ENV_VARS {
            { "MAX_INSERTS", "The max number of inserts to perform while running a randomized test" },
            { "RUN_DIAGNOSTICS", "Print data structures and statistics while running tests" },
            END_ENV_VARS
        },
        CMDS {
             {
                BTREE,
                "btree",
                ARGS {
                    help_and_version_args,
                    END_ARGS
                },
                ENV_VARS {
                    { "WITH_NILS", "Print the nil (black) nodes" },
                    { "SIZE", "Generate a random tree with SIZE nodes in ascending order" },
                    { "RANDOM", "Randomize the key order" },
                    END_ENV_VARS
                },
                NULL,
                "Print dot for a randomly generated red-black tree"
            },
            END_CMDS
        },
        "Run the test suite"
    });

    if (args.cmd == BTREE) {
        graph_int_tree();
        return 0;
    } else {
        int number_failed;

        SRunner *sr = srunner_create(NULL);
        srunner_add_suite(sr, array_suite());
        srunner_add_suite(sr, btree_suite());
        srunner_add_suite(sr, hash_table_suite());
        srunner_add_suite(sr, intset_suite());
        srunner_run_all(sr, CK_ENV);
        number_failed = srunner_ntests_failed(sr);
        srunner_free(sr);

        return number_failed == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
}
