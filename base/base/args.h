#ifndef BASE_ARGS_H_
#define BASE_ARGS_H_ 1

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <getopt.h>
#include "base/array.h"

#define OPTIONS_SIZE 64
#define OPTSTRING_SIZE 128
#define ARGMAX 32
#define CMDMAX 10

struct arg {
    int key;
    char *lname;
    char sname;
    int has_val;
    char *desc;
};

struct cmd {
    int key;
    char *cmd;
    struct arg *args;
    struct cmd *subcmds;
    char *desc;
};

struct val_assoc {
    int key;
    int val;
};

struct args_handlers {
    void (*cmd_not_found) (struct args_context *context);
    void (*help_found) (struct args_context *context);
    void (*version_found) (struct args_context *context);
    void (*missing_arg_found) (struct args_context *context);
    void (*unknown_option_found) (struct args_context *context);
};

struct args_context {
    struct cmd *cmd;
    char *version;
    struct args_handlers
    struct cmd **cmd_path;
    int argc;
    char **argv;
    int flag;
    char *optstring;
    struct option *options;
    struct val_assoc *val_table;
};

enum {
    END = -1,
    HELP = -2,
    VERSION = -3,
    UNKNOWN_OPTION -4,
    MISSING_ARG -5
};

#define help_arg { HELP, "help", 0, no_argument, "Print help" }
#define version_arg { VERSION, "version", 0, no_argument, "Print version" }
#define help_and_version_args help_arg, version_arg
#define end_arg { DONE, NULL, 0, no_argument, NULL }
#define end_cmd { DONE, NULL, 0, NULL, NULL }
#define ARG_FN (void (*) (void *, int, struct args_context *))
#define CMD &(struct cmd)
#define CMDS (struct cmd[])
#define ARGS (struct arg[])
#define default_handlers NULL

void run_args(
    void *out_val,
    void (*read_args) (void *out_val, int cmd, struct args_context *context),
    char *version,
    int argc, char **argv,
    struct args_handlers *handlers,
    struct cmd *cmd
);
int readarg(struct args_context context);
char *argval();
struct argv argv(struct args_context *context);
void print_usage(FILE *handle, struct args_context *context);
void cmd_not_found(struct args_context *context);
void help_found(struct args_context *context);
void version_found(struct args_context *context);
void missing_arg_found(struct args_context *context);
void unkown_option_found(struct args_context *context);

struct args_context init_args_context(struct cmd **cmd_path, char *version, struct argv argv);
struct args_handlers default_handlers();
void determine_options(struct args_context *context, char *optstring, struct option *options, struct val_assoc *val_table);
void findcmd(struct args_context *context);
struct option arg_to_option(struct arg arg, int *flag);

#endif // BASE_ARGS_H_

