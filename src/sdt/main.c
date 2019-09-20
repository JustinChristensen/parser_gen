#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <base/macros.h>
#include <base/list.h>
#include <base/args.h>
#include "gensource.h"
#include "scanner.h"
#include "parser.h"
#include "source.h"
#include "dot.h"

enum command_key {
    PARSE,
    GENERATE
};

enum arg_key {
    FORMAT
}

enum output_fmt {
    OUTPUT_AST,
    OUTPUT_SOURCE,
    OUTPUT_TOKENS
};

struct args {
    enum command_key cmd;
    enum output_fmt output;
    int pos_size;
    char **pos;
};

void output_ast(struct program *ast, char *input, enum output_fmt format) {
    if (format == OUTPUT_AST) {
        print_dot(stdout, ast, input, TOGRAPHFN program_to_graph);
    } else {
        print_source(stdout, ast, TOSOURCEFN program_to_source);
    }
}

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != DONE) {
        switch (cmd) {
            case PARSE:
            case GENERATE:
                switch (k) {
                    case FORMAT:
                        char *val = argval(reader);
                        if (strcmp("ast", val) == 0) {
                            args->output = OUTPUT_AST;
                        } else if (strcmp("source", val) == 0) {
                            args->output = OUTPUT_SOURCE;
                        } else if (strcmp("tokens", val) == 0) {
                            args->output = OUTPUT_TOKENS;
                        }
                        break;
                }
                break;
        }
    }

    args->pos = argv(context);
    args->pos_size = argc(context);

    return args;
}

#define BUFFER_SIZE 1000000

int main(int argc, char *argv[]) {
    struct cmd cmds = {
        PARSE,
        NULL,
        {
            help_and_version,
            { FORMAT, "format", "f", required_argument, true, "Output format: ast, input, or tokens" },
            end_arg
        },
        {
            { GENERATE, "generate", NULL, NULL, "Generate source" },
            end_cmd
        },
        "Parse the source program"
    };

    struct args args = {
        .cmd = PARSE,
        .output = OUTPUT_SOURCE
        .pos_size = 0,
        .pos = NULL,
    };

    run_args_reader(&args, &cmds, "1.0.0", argc, argv, cmd_not_found, ARG_READER_FN read_args);

    if (args.cmd == GENERATE) {
        struct gendims dims = {
            .minw = 3,
            .maxw = 10,
            .mind = 30,
            .maxd = 40
        };
        struct program *program = gen_program(dims);
        output_ast(program, NULL, args.output);
        free_program(program);
    } else {
        FILE *in = NULL;

        if (args.pos_size == 0) {
            if (!isatty(STDIN_FILENO)) {
                in = stdin;
            }
        } else {
            in = fopen(args.pos[0], "r");
        }

        if (in) {
            char input[BUFFER_SIZE] = "";
            size_t nread = fread(input, sizeof *input, BUFFER_SIZE, in);
            input[nread] = '\0';

            if (args.output == OUTPUT_TOKENS) {
                struct list *tokens_ = tokens(input);

                for (struct node *node = head(tokens_); node; node = next(node)) {
                    display_token(value(node));
                    printf("\n");
                }

                free_list(tokens_, VOIDFN1 free_token);
            } else {
                struct parse_context context = parse_context(input);
                struct program *ast;

                if ((ast = program(&context))) {
                    output_ast(ast, input, args.output);
                    free_parse_context(&context);
                } else {
                    display_parse_error(&context);
                    free_parse_context(&context);
                    return EXIT_FAILURE;
                }
            }

            fclose(in);
        } else {
            fprintf(stderr, "no input file\n");
        }
    }

    return EXIT_SUCCESS;
}
