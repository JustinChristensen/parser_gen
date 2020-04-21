#ifndef BASE_ARGS_H_
#define BASE_ARGS_H_ 1

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <getopt.h>

#ifndef SUBCMD_HELP_FMT
#define SUBCMD_HELP_FMT "  %-24s  %s"
#endif

#ifndef ARG_HELP_FMT
#define ARG_HELP_FMT "  %-24s  %s"
#endif

#ifndef VAR_HELP_FMT
#define VAR_HELP_FMT "  %-24s  %s"
#endif

#ifndef OPTIONS_SIZE
#define OPTIONS_SIZE 128
#endif

#ifndef OPSTRING_SIZE
#define OPTSTRING_SIZE 256
#endif

struct arg {
    int key;
    char *lname;
    char sname;
    int has_val;
    char *desc;
};

struct env_var {
    char *name;
    char *desc;
};

struct cmd {
    int key;
    char *cmd;
    struct arg *args;
    struct env_var *vars;
    struct cmd *subcmds;
    char *desc;
};

struct val_assoc {
    int key;
    int val;
};

struct args_context {
    struct cmd *cmd;
    char *version;
    struct args_handlers *handlers;
    struct cmd **cmd_path;
    int argc;
    char **argv;
    int flag;
    char *optstring;
    struct option *options;
    struct val_assoc *val_table;
};

struct args_handlers {
    void (*help_found) (struct args_context *context);
    void (*version_found) (struct args_context *context);
    void (*missing_arg_found) (struct args_context *context);
    void (*unknown_option_found) (struct args_context *context);
};

enum {
    END             = -1,
    HELP            = -2,
    VERSION         = -3,
    UNKNOWN_OPTION  = -4,
    MISSING_ARG     = -5
};

#define ARG_FN (void (*) (void *, int, struct args_context *))
#define CMD &(struct cmd)
#define CMDS (struct cmd[])
#define ARGS (struct arg[])
#define ENV_VARS (struct env_var[])

static struct arg const help_arg = { HELP, "help", 0, no_argument, "Print help" };
static struct arg const version_arg = { VERSION, "version", 0, no_argument, "Print version" };
#define help_and_version_args help_arg, version_arg
static struct arg const END_ARGS = { END, NULL, 0, no_argument, NULL };
static struct cmd const END_CMDS = { END, NULL, 0, NULL, NULL };
static struct env_var const END_ENV_VARS = { NULL, NULL };

void run_args(
    void *out_val,
    void (*read_args) (void *out_val, int cmd, struct args_context *context),
    char *version,
    int argc, char **argv,
    struct args_handlers *handlers,
    struct cmd *cmd
);
int readarg(struct args_context *context);
char *argval();
char **argv(struct args_context *context);
int argc(struct args_context *context);
void print_usage(FILE *handle, struct args_context *context);
void print_version(struct args_context *context);
void help_found(struct args_context *context);
void version_found(struct args_context *context);
void missing_arg_found(struct args_context *context);
void unknown_option_found(struct args_context *context);

struct args_context init_args_context(
    struct cmd *cmd, char *version,
    struct args_handlers *handlers,
    struct cmd **cmd_path,
    int argc, char **argv
);
void determine_options(struct args_context *context, char *optstring, struct option *options, struct val_assoc *val_table);
void findcmd(struct args_context *context);
struct option arg_to_option(struct arg *arg, int *flag);

#endif // BASE_ARGS_H_
