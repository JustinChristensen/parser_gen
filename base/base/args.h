#ifndef BASE_ARGS_H_
#define BASE_ARGS_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include "base/array.h"

struct arg {
    int key;
    char *lname;
    char *sname;
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

struct arg_reader {
    struct cmd *cmd;
    char *version;
    int argc;
    char *argv;
};

enum {
    DONE = -1,
    HELP = -2,
    VERSION = -3
};

#define help_arg { HELP, "help", NULL, no_argument, "Print help" }
#define version_arg { VERSION, "version", NULL, no_argument, "Print version" }
#define help_and_version help_arg, version_arg

struct arg_reader arg_reader(struct cmd *cmds, char *version, int argc, char **argv);
int findcmd(struct args_context context);
int readarg(struct args_context context);
char *argval();
int argc(struct args_context context);
int argv(struct args_context context);
void print_usage(FILE *handle, struct args_context context);

#endif // BASE_ARGS_H_

