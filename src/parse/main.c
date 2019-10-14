#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>

enum command_key {
    GRAM,
    GEN_LL,
    GEN_LR
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

int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = GRAM,
        .posc = 0,
        .pos = NULL,
    };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        GRAM,
        NULL,
        ARGS {
            help_and_version_args,
            END_ARGS
        },
        CMDS {
            {
                GEN_LL,
                "genll",
                ARGS {
                    help_and_version_args,
                    END_ARGS
                },
                NULL,
                "Generate an LL parser for the input lexer and grammar specification"
            },
            END_CMDS
        },
        "Analyze a grammar and generate a report"
    });

    return EXIT_SUCCESS;
}
