#include "base/args.h"
#include "base/array.h"
#include "base/tuple.h"
#include <stdbool.h>

struct arg_reader arg_reader(struct cmd *cmds, char *version, int argc, char **argv) {
    struct arg_reader context = {
        .cmd = cmds,
        .version = version,
        .argv = argv,
        .argc = argc
    };

    return context;
}

int findcmd(struct args_context context) {
    if (context.argc > 2) {
        int argc = context.argc;
        char **argv = context.argv;
        char *prog = context.argv[0];
        struct cmd *subcmds = context.cmd->subcmds;

        while (subcmds && *argv[1] != '-') {
            size_t nsubs = sizeof(subcmds) / sizeof (subcmds[0]);
            struct cmd *found = NULL;

            for (int i = 0; i < nsubs; i++) {
                if (strcmp(subcmds[i].cmd, argv[1]) == 0) {
                    found = subcmds[i];
                    ++argv = prog;
                    argc--;
                }
            }

            if (found) subcmds = found->subcmds;

            context.cmd = found;
        }

        context.argc = argc;
        context.argv = argv;
    }

    if (!context.cmd) {
        return print_usage(stderr, context);
    }

    return context.cmd.key;
}

int readarg(struct args_context context) {
}

char *argval() {
    return optarg;
}

int argc(struct args_context context) {
    return context.argc;
}

int argv(struct args_context context) {
    return context.argv;
}

#define FLAG_INDENT "   "
#define DESC_SEP "    "

void print_usage(char *prog_name, FILE *handle) {
    fprintf(handle, "usage: %-s [options]\n\n", prog_name);
    // fprintf(handle, "subcommands:\n");
    // fprintf(handle, "%-s%s%-24s%-s\n", FLAG_INDENT, "generate", DESC_SEP, "generate source");
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

struct args read_args(int argc, char *argv[]) {
    struct args args = {
        .pos_size = 0,
        .pos = NULL,
        .cmd = PARSE,
        .output = OUTPUT_SOURCE,
    };

    // if (argc > 0) {
    //     if (strcmp("generate", argv[1]) == 0) {
    //         char *prog = argv[0];
    //         args.cmd = GENERATE;
    //         argc--;
    //         *argv++ = prog;
    //     }
    // }

    int f;
    while ((f = getopt_long(argc, argv, "fh", options(opt_descs), NULL)) != -1) {
            case 'h':
                print_usage(argv[0], stdout);
                exit(EXIT_SUCCESS);
                break;
            case VERSION:
                print_version();
                exit(EXIT_SUCCESS);
                break;
    }

                default:
                    print_usage(argv[0], stderr);
                    exit(EXIT_FAILURE);
    args.pos_size = argc - optind;
    args.pos = argv + optind;

    return args;
}

