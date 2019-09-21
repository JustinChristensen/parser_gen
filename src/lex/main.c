#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>

enum command_key {
    LEX
};

struct args {
    enum command_key cmd;
    int pos_size;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END);
    args->cmd = cmd;
    args->pos = argv(context);
    args->pos_size = argc(context);
}

int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = LEX,
        .pos_size = 0,
        .pos = NULL,
    };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        LEX,
        NULL,
        ARGS {
            help_and_version_args,
            end_arg
        },
        NULL,
        "Run the lexer"
    });

    return EXIT_SUCCESS;
}
