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

#define BUFFER_SIZE 1000000

struct args read_args(int argc, char **argv) {
    struct arg format_arg = { FORMAT, "format", "f", required_argument, "Output format: ast, input, or tokens" };

    struct cmd cmds = {
        PARSE,
        NULL,
        { help_and_version, format_arg },
        {
            {
                GENERATE,
                "generate",
                "Generate source",
                { help_and_version, format_arg },
                NULL
            }
        },
        "Parse the source program"
    };

    struct arg_reader reader = arg_reader(&cmds, "1.0.0", argc, argv);

    struct args args = {
        .cmd = PARSE,
        .output = OUTPUT_SOURCE
        .pos_size = 0,
        .pos = NULL,
    };

    args.cmd = findcmd(reader);

    int key;
    while ((key = readarg(reader)) != DONE) {
        switch (args.cmd) {
            case PARSE:
            case GENERATE:
                switch (k) {
                    case FORMAT:
                        char *val = argval(reader);
                        if (strcmp("ast", val) == 0) {
                            args.output = OUTPUT_AST;
                        } else if (strcmp("source", val) == 0) {
                            args.output = OUTPUT_SOURCE;
                        } else if (strcmp("tokens", val) == 0) {
                            args.output = OUTPUT_TOKENS;
                        }
                        break;
                }
                break;
        }
    }

    args.pos = argv(reader);
    args.pos_size = argc(reader);

    return args;
}

int main(int argc, char *argv[]) {
    struct args args = read_args(argc, argv);

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
