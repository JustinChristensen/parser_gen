#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <base/args.h>
#include <base/graphviz.h>
#include "ast.h"
#include "dot.h"
#include "parser.h"
#include "print_ast.h"

enum command_key {
    AUTO,
    PRINT
};

enum arg_key {
    FORMAT
};

enum output_fmt {
    OUTPUT_DOT,
    OUTPUT_TREE
};

struct args {
    enum command_key cmd;
    enum output_fmt output;
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END) {
        switch (cmd) {
            case PRINT:
                switch (key) {
                    case FORMAT:
                        if (strcmp("dot", argval()) == 0) {
                            args->output = OUTPUT_DOT;
                        } else if (strcmp("tree", argval()) == 0) {
                            args->output = OUTPUT_TREE;
                        } else {
                            print_usage(stderr, context);
                            exit(EXIT_FAILURE);
                        }
                        break;
                }
                break;
        }
    }
    args->cmd = cmd;
    args->pos = argv(context);
    args->posc = argc(context);
}

int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = AUTO,
        .output = OUTPUT_TREE,
        .posc = 0,
        .pos = NULL,
    };

    struct arg fmt_arg = { FORMAT, "format", 'f', optional_argument, "Output format: dot or tree" };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        AUTO,
        NULL,
        ARGS {
            help_and_version_args,
            end_arg
        },
        CMDS {
             {
                PRINT,
                "print",
                ARGS {
                    help_and_version_args,
                    fmt_arg,
                    end_arg
                },
                NULL,
                "Print the syntax tree for each regular expression"
            },
            end_cmd
        },
        "Construct and simulate automata"
    });

    if (args.cmd == PRINT && args.posc > 0) {
        struct expr exprbuf[EXPR_MAX];
        struct parse_context context = parse_context(args.pos[0], exprbuf);

        if (parse_regex(&context)) {
            struct expr *expr = gexpr(&context);

            if (args.output == OUTPUT_TREE) {
                printf("expr type: %d\n", expr->type);
                printf("constructed %ld expressions\n", context.exprbuf - exprbuf);
                print_expr(expr);
            } else {
                print_dot(stdout, expr, NULL, TOGRAPHFN regex_to_graph);
            }
        } else {
            print_error(gerror(&context));
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}
