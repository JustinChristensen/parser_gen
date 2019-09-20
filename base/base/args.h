#ifndef BASE_ARGS_H_
#define BASE_ARGS_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <getopt.h>
#include "base/array.h"

struct arg {
    int key;
    char *lname;
    char *sname;
    int has_val;
    bool inheritable;
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

struct args_context {
    struct cmd **cmd_path;
    char *version;
    int argc;
    char **argv;
    char *optstring;
    struct option *options;
    struct val_assoc *val_table;
};

enum {
    DONE = -1,
    HELP = -2,
    VERSION = -3
};

#define help_arg { HELP, "help", NULL, no_argument, true, "Print help" }
#define version_arg { VERSION, "version", NULL, no_argument, true, "Print version" }
#define help_and_version help_arg, version_arg
#define end_arg { DONE, NULL, NULL, no_argument, false, NULL }
#define end_cmd { DONE, NULL, NULL, NULL, NULL, NULL }
#define ARG_READER_FN (void (*) (void *, int, struct args_context *))
#define OPTIONS_SIZE 64
#define OPTSTRING_SIZE 128

struct argv run_args_reader(
    void *out_val,
    struct cmd *cmd, char *version,
    int argc, char **argv
    void (*cmd_not_found) (struct args_context *context)
    void (*read_args) (void *out_val, int cmd, struct args_context *context)
);
int readarg(struct args_context context);
char *argval();
struct argv argv(struct args_context *context);
void print_usage(FILE *handle, struct args_context *context);
void cmd_not_found(struct args_context *context);

struct args_context init_args_context(struct cmd **cmd_path, char *version, struct argv argv);
void determine_options(struct args_context *context);
int findcmd(struct args_context *context, struct cmd *cmd);
struct option arg_to_option(struct arg arg, int *flag);

#endif // BASE_ARGS_H_

