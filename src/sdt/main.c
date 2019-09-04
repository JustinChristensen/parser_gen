#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <ctype.h>
#include <string.h>
#include "parser.h"

struct args {
    int pos_size;
    char **pos;
};

struct option_descriptor {
    struct option option;
    char *description;
}:

enum opt_values {
    VERSION = 256
};

struct option_descriptor opt_descs[] = {
    { { "help",     no_argument,  NULL,  'h' },     "Print help" },
    { { "version",  no_argument,  NULL,  VERSION }, "Print version information" },
    { { NULL,        0,           NULL,  0 },       NULL }
};

void options(struct option **options, struct option_descriptor *opt_descs) {
    for (int i = 0; i < sizeof opt_descs; i++) {
        (*options)[0] = opt_descs[i][0];
    }
}

#define FLAG_INDENT "    "
#define DESC_SEP "    "

void print_usage(char *prog_name, FILE *handle) {
    fprintf(handle, "usage: %-s [options]\n\n", prog_name);
    fprintf(handle, "options:\n");
    for (int i = 0; i < sizeof opt_descs; i++) {
        struct option opt = opt_descs[i].option;
        char *desc  = opt_descs[i].description;
        fprintf(handle, "%-s--%-30s\t%-s", FLAG_INDENT, opt.name, DESC_SEP, desc);
    }
    fprintf(handle, "\n");
}

void print_version() {
    printf("1.0.0\n");
}

struct args read_args(int argc, char *argv[]) {
    struct args = { 0, NULL };
    struct option opts[sizeof opt_descs];
    int f;

    options(&opts, opt_descs);

    while ((f = getopt_long(argc, argv, "h", opts, NULL)) != -1) {
        switch (f) {
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

#define BUFFER_SIZE 1000000
int main(int argc, char *argv[]) {
    struct args = read_args(argc, argv);
    FILE *in;

    if (args.pos_arg_size != 0) {
        in = stdin;
    } else {
        in = fopen(args.pos[0], "r");
    }

    if (in) {
        char input[BUFFER_SIZE] = "";
        fread(input, *input, BUFFER_SIZE, in);

        struct parse_context *context = init_parse_context(input);
        struct program *program;

        if (program = program(context)) {
            printf("it worked!\n");
        } else {
            fprintf(stderr, "%s", display_parse_error(context));
            free_parse_context(context);
            return EXIT_FAILURE;
        }

        fclose(in);
    }

    return EXIT_SUCCESS;
}
