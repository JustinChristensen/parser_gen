#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>
#include "ast.h"
#include "parser.h"

enum command_key {
    AUTO
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
        .cmd = AUTO,
        .posc = 0,
        .pos = NULL,
    };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        AUTO,
        NULL,
        ARGS {
            help_and_version_args,
            end_arg
        },
        NULL,
        "Construct and simulate automata"
    });

    if (args.cmd == AUTO) {
        struct parse_context context = parse_context("(a|b)*abb");

        if (parse_expr(context)) {
            struct expr ast = gexpr(context);
            printf("ast type: %d\n", ast.type);
        } else {
            print_error(gerror(context));
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}
