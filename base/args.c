#include "base/args.h"
#include "base/array.h"
#include "base/tuple.h"
#include <stdbool.h>

struct args_context init_args(char *version, struct args_handlers handlers, struct cmd *cmd) {
    return (struct args_context) {
        .cmd = cmd,
        .version = version,
        .handlers = handlers,

        .cmd_path = NULL,
        .argc = 0,
        .argv = NULL
    };
}

void run_args(
    void *out_val,
    void (*read_args) (void *out_val, int cmd, struct args_context *context),
    char *version,
    int argc, char **argv,
    struct args_handlers *handlers,
    struct cmd *cmd
) {
    // if handlers is null, use default handlers
    // if read args
    //      create the context
    //      find the command, storing the command path for later (when printing usage), i.e. rails generate model
    //      if the command was found
    //          initialize getopt related storage (optstring, options, etc)
    //          compute the getopt stuff
    //          run read args with the out value, found command key, and context
    //      if the command was not found
    //          run the command not found handler
    //
    //  read args
    //      while read arg
    //          if arg has a handler (help, version, missing arg, unknown option), run the handler
    //          else, return arg key to caller

    if (handlers.read_args) {
        struct args_context context = init_args_context(&cmd, version, argc, argv);

        findcmd(&context);

        if (context.cmd) {
            char optstring[OPTSTRING_SIZE] = "";
            struct option options[OPTIONS_SIZE];
            struct val_assoc val_table[OPTIONS_SIZE];

            determine_options(&context, optstring, options, val_table);

            (*handlers.read_args)(out_val, context.cmd.key, &context);
        } else if (cmd_not_found) {
            (*handlers.cmd_not_found)(&context);
        }
    }
}

struct args_handlers default_handlers() {
    return (struct args_handlers) {
        .cmd_not_found = cmd_not_found,
        .help_found = help_found,
        .version_found = version_found,
        .missing_arg_found = cmd_not_found,
        .unknown_option_found = cmd_not_found
    };
}

void findcmd(struct args_context *context) {
    if (context->argc > 2) {
        struct cmd *cmd = context->cmd;
        int argc = context->argc;
        char **argv = context->argv;
        char *prog = argv[0];

        while (cmd && *argv[1] != '-') {
            struct cmd *sub = cmd->subcmds;
            cmd = NULL;

            while (sub && sub->key != END) {
                if (strcmp(sub->cmd, argv[1]) == 0) {
                    cmd = sub
                    argv++;
                    argc--;
                    argv[0] = prog;
                    break;
                }

                sub++;
            }
        }

        context->argc = argc;
        context->argv = argv;
        context->cmd = cmd;
    }
}

struct option arg_to_option(struct arg arg, int *flag) {
    return (struct option) {
        .name = arg.lname,
        .has_arg = arg.has_val,
        .flag = flag,
        .val = arg.key
    };
}

void determine_options(struct args_context *context, char *optstring, struct option *options, struct val_assoc *val_table) {
    struct arg *arg = context->cmd->args;

    while (arg && arg->key != END) {
        *options++ = arg_to_option(arg, &context->flag);

        if (arg->sname) {
            *optstring++ = arg->sname;

            if (arg->has_val == optional_argument || arg->has_val == required_argument) {
                *optstring++ = ':';
            }

            struct val_assoc va = { arg->sname, arg->key };
            *val_table++ = va
        }

        arg++;
    }

    context->optstring = optstring;
    context->options = options;
    context->val_table = val_table;
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

void help_found(struct args_context *context) {
    print_usage(stdout, context);
    exit(EXIT_SUCCESS);
}

void version_found(struct args_context *context) {
    print_version(context);
    exit(EXIT_SUCCESS);
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

