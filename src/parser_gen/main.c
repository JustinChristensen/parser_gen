#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>

enum command_key {
    GEN_PARSER
};

// enum arg_key {
// };

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
        .cmd = GEN_PARSER
    };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        GEN_PARSER,
        NULL,
        ARGS {
            help_and_version_args,
            END_ARGS
        },
        NULL,
        "Generate a parser"
    });

    return EXIT_SUCCESS;
}
