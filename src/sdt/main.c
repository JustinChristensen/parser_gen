#include <stdio.h>
#include <stdlib.h>
#include <macros.h>
#include <getopt.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <linked_list.h>
#include "scanner.h"
#include "parser.h"
#include "dot.h"

struct args {
    int pos_size;
    char **pos;
    bool scan_only;
    bool output_dot;
};

struct option_descriptor {
    struct option option;
    char *description;
};

enum opt_values {
    VERSION = 256,
    SCAN_ONLY,
    OUTPUT_DOT
};

#define NUM_DESCS 5
static struct option_descriptor opt_descs[] = {
    { { "help",      no_argument,  NULL,  'h' },        "Print help" },
    { { "version",   no_argument,  NULL,  VERSION },    "Print version information" },
    { { "scan-only", no_argument,  NULL,  SCAN_ONLY },  "Print the result of scanning the input stream" },
    { { "dot",       no_argument,  NULL,  OUTPUT_DOT }, "Print the resulting AST as dot" },
    { { NULL,         0,           NULL,  0 },          NULL }
};

static struct option opts[NUM_DESCS];

struct option *options(struct option_descriptor *opt_descs) {
    for (int i = 0; i < NUM_DESCS; i++) {
        opts[i] = opt_descs[i].option;
    }

    return opts;
}

#define FLAG_INDENT " "
#define DESC_SEP "    "

void print_usage(char *prog_name, FILE *handle) {
    fprintf(handle, "usage: %-s [options]\n\n", prog_name);
    fprintf(handle, "options:\n");
    for (int i = 0; i < NUM_DESCS; i++) {
        char *desc  = opt_descs[i].description;
        if (desc) {
            struct option opt = opt_descs[i].option;
            fprintf(handle, "%-s--%-24s%-s%-s\n", FLAG_INDENT, opt.name, DESC_SEP, desc);
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
        .scan_only = false,
        .output_dot = false
    };
    int f;
    while ((f = getopt_long(argc, argv, "h", options(opt_descs), NULL)) != -1) {
        switch (f) {
            case 'h':
                print_usage(argv[0], stdout);
                exit(EXIT_SUCCESS);
                break;
            case VERSION:
                print_version();
                exit(EXIT_SUCCESS);
                break;
            case SCAN_ONLY:
                args.scan_only = true;
                break;
            case OUTPUT_DOT:
                args.output_dot = true;
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

#define BUFFER_SIZE 1000000
int main(int argc, char *argv[]) {
    struct args args = read_args(argc, argv);
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

        if (args.scan_only && args.output_dot) {
            fprintf(stderr, "warning: both --scan-only and --dot provided, preferring --scan-only\n");
        }

        if (args.scan_only) {
            struct list *tokens_ = tokens(input);

            for (struct node *node = head(tokens_); node; node = node->next) {
                display_token(value(node));
                printf("\n");
            }

            free_list(tokens_, VOIDFN1 free_token);
        } else {
            struct parse_context context = parse_context(input);
            struct program *ast;

            if ((ast = program(&context))) {
                if (args.output_dot) {
                    print_dot(stdout, ast, TOGRAPHFN program_to_graph);
                } else {
                    printf("it worked!\n");
                }
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

    return EXIT_SUCCESS;
}
