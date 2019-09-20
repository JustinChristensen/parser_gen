#include "base/args.h"
#include "base/array.h"
#include "base/tuple.h"
#include <stdbool.h>

struct args_context init_args_context(struct cmd **cmd_path, char *version, int argc, char **argv) {
    struct args_context context = {
        .cmd_path = cmd_path,
        .version = version,
        .argc = argc,
        .argv = argv,
        .optstring = NULL,
        .options = NULL,
        .val_table = NULL
    };

    return context;
}

struct argv run_args_reader(
    void *out_val,
    struct cmd *cmd, char *version,
    int argc, char **argv,
    void (*cmd_not_found) (struct args_context *context),
    void (*read_args) (void *out_val, struct args_context *context)
) {
    if (read_args) {
        struct cmd *cmd_path[16] = { NULL };
        struct args_context context = init_args_context(cmd_path, version, argc, argv);
        int cmdkey = findcmd(&context, cmds);

        if (cmdkey != DONE) {
            char optstring[OPTSTRING_SIZE] = "";
            struct option options[OPTIONS_SIZE];
            struct val_assoc val_table[OPTIONS_SIZE];

            context.optstring = optstring;
            context.options = options;
            context.val_table = val_table;

            determine_options(&context);

            (*read_args)(out_val, cmdkey, &context);
        } else if (cmd_not_found) {
            (*cmd_not_found)(&context);
        }
    }
}

int findcmd(struct args_context *context, struct cmd *cmd) {
    int cmdkey = cmd.key;
    struct **cmd_path = context->cmd_path;
    *cmd_path++ = cmd;

    if (context->argc > 2) {
        int argc = context->argc;
        char **argv = context->argv;
        char *prog = argv[0];

        while (cmd && *argv[1] != '-') {
            struct cmd *subcmds = cmd->subcmds;

            if (subcmds) {
                while ((*subcmds).key != DONE) {
                    if (strcmp(*subcmds.cmd, argv[1]) == 0) {
                        cmd = *subcmds;
                        *cmd_path++ = cmd;
                        argv++;
                        argc--;
                        argv[0] = prog;
                    }
                }
            } else {
                cmd = NULL;
                cmdkey = DONE;
            }
        }

        context->argc = argc;
        context->argv = argv;
    }

    return cmdkey;
}

struct option arg_to_option(struct arg arg, int *flag) {
    struct option opt = {
        .name = arg.lname,
        .has_arg = arg.has_val,
        .flag = flag,
        .val = arg.key
    };

    return opt;
}

void determine_options(struct args_context *context) {
}

int readarg(struct arg_reader reader) {
}

char *argval() {
    return optarg;
}

int argc(struct arg_reader reader) {
    return reader.argc;
}

int argv(struct arg_reader reader) {
    return reader.argv;
}

void cmd_not_found(struct args_context *context) {
    print_usage(stderr, context);
    exit(EXIT_FAILURE);
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

