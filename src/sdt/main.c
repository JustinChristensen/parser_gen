#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <base/macros.h>
#include <base/linked_list.h>
#include "gensource.h"
#include "scanner.h"
#include "parser.h"
#include "source.h"
#include "dot.h"

enum cmd {
    PARSE,
    GENERATE
};

enum output_fmt {
    OUTPUT_AST,
    OUTPUT_SOURCE,
    OUTPUT_TOKENS
};

struct args {
    int pos_size;
    char **pos;
    enum cmd cmd;
    enum output_fmt output;
};

struct option_descriptor {
    struct option option;
    char *description;
};

enum opt_values {
    VERSION = 256
};

#define NUM_DESCS 4
static struct option_descriptor opt_descs[] = {
    { { "format",    required_argument,  NULL,  'f' },        "Output format: ast, input, or tokens" },
    { { "version",   no_argument,        NULL,  VERSION },    "Print version information" },
    { { "help",      no_argument,        NULL,  'h' },        "Print help" },
    { { NULL,         0,                 NULL,  0 },          NULL }
};

static struct option opts[NUM_DESCS];

struct option *options(struct option_descriptor *opt_descs) {
    for (int i = 0; i < NUM_DESCS; i++) {
        opts[i] = opt_descs[i].option;
    }

    return opts;
}

#define FLAG_INDENT "   "
#define DESC_SEP "    "

void print_usage(char *prog_name, FILE *handle) {
    fprintf(handle, "usage: %-s [options]\n\n", prog_name);
    fprintf(handle, "subcommands:\n");
    fprintf(handle, "%-s%s%-24s%-s\n", FLAG_INDENT, "generate", DESC_SEP, "generate source");
    fprintf(handle, "\n");
    fprintf(handle, "options:\n");
    for (int i = 0; i < NUM_DESCS; i++) {
        char *desc  = opt_descs[i].description;
        if (desc) {
            struct option opt = opt_descs[i].option;
            char flag[2] = { opt.val, '\0' };
            const char *f = opt.name, *d = "--";
            if (f == NULL) {
                f = flag;
                d = " -";
            }
            fprintf(handle, "%-s%s%-24s%-s%-s\n", FLAG_INDENT, d, f, DESC_SEP, desc);
        }
    }
    fprintf(handle, "\n");
}

void print_version() {
    printf("1.0.0\n");
}

struct args read_args(int argc, char *argv[]) {
    struct args args = {
        .pos_size = 0,
        .pos = NULL,
        .cmd = PARSE,
        .output = OUTPUT_SOURCE,
    };

    if (argc > 0) {
        if (strcmp("generate", argv[1]) == 0) {
            char *prog = argv[0];
            args.cmd = GENERATE;
            argc--;
            *argv++ = prog;
        }
    }

    int f;
    while ((f = getopt_long(argc, argv, "fh", options(opt_descs), NULL)) != -1) {
        switch (f) {
            case 'f':
                if (strcmp("ast", optarg) == 0) {
                    args.output = OUTPUT_AST;
                } else if (strcmp("source", optarg) == 0) {
                    args.output = OUTPUT_SOURCE;
                } else if (strcmp("tokens", optarg) == 0) {
                    args.output = OUTPUT_TOKENS;
                }
                break;
            case 'h':
                print_usage(argv[0], stdout);
                exit(EXIT_SUCCESS);
                break;
            case VERSION:
                print_version();
                exit(EXIT_SUCCESS);
                break;
            default:
                print_usage(argv[0], stderr);
                exit(EXIT_FAILURE);
        }
    }

    args.pos_size = argc - optind;
    args.pos = argv + optind;

    return args;
}

void output_ast(struct program *ast, char *input, enum output_fmt format) {
    if (format == OUTPUT_AST) {
        print_dot(stdout, ast, input, TOGRAPHFN program_to_graph);
    } else {
        print_source(stdout, ast, TOSOURCEFN program_to_source);
    }
}

#define BUFFER_SIZE 1000000
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
