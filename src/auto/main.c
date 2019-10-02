#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <base/args.h>
#include <base/stack.h>
#include <base/graphviz.h>
#include "ast.h"
#include "nfa.h"
#include "dot.h"
#include "parser.h"
#include "print_ast.h"

enum command_key {
    AUTO,
    PRINT,
    NFA,
    NFA_TABLE,
    NFA_DOT
};

enum arg_key {
    FORMAT,
};

enum output_fmt {
    OUTPUT_DOT,
    OUTPUT_TREE
};

struct args {
    enum command_key cmd;
    enum output_fmt output;
    bool state_table;
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END) {
        switch (cmd) {
            case PRINT:
                switch (key) {
                    case FORMAT:
                        if (strcmp("dot", argval()) == 0) {
                            args->output = OUTPUT_DOT;
                        } else if (strcmp("tree", argval()) == 0) {
                            args->output = OUTPUT_TREE;
                        } else {
                            print_usage(stderr, context);
                            exit(EXIT_FAILURE);
                        }
                        break;
                }
                break;
        }
    }
    args->cmd = cmd;
    args->pos = argv(context);
    args->posc = argc(context);
}

#define BUFFER_SIZE 4096
int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = AUTO,
        .output = OUTPUT_TREE,
        .state_table = false,
        .posc = 0,
        .pos = NULL,
    };

    struct arg fmt_arg = { FORMAT, "format", 'f', required_argument, "Output format: dot or tree" };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        AUTO,
        NULL,
        ARGS {
            help_and_version_args,
            END_ARGS
        },
        CMDS {
             {
                PRINT,
                "print",
                ARGS {
                    help_and_version_args,
                    fmt_arg,
                    END_ARGS
                },
                NULL,
                "Print the syntax tree for each regular expression"
            },
            {
                NFA,
                "nfa",
                ARGS {
                    help_and_version_args,
                    END_ARGS
                },
                CMDS {
                    {
                        NFA_TABLE,
                        "table",
                        ARGS {
                            help_and_version_args,
                            END_ARGS
                        },
                        NULL,
                        "Print the state table for the nfa"
                    },
                    {
                        NFA_DOT,
                        "dot",
                        ARGS {
                            help_and_version_args,
                            END_ARGS
                        },
                        NULL,
                        "Print dot"
                    },
                    END_CMDS
                },
                "Construct and simulate an NFA"
            },
            END_CMDS
        },
        "Construct and simulate automata"
    });

    if (!isatty(STDIN_FILENO)) {
        char input[BUFFER_SIZE] = "";
        size_t nread = fread(input, sizeof *input, BUFFER_SIZE, stdin);
        input[nread] = '\0';

        if (args.cmd == PRINT) {
            struct expr exprbuf[EXPR_MAX];
            struct expr_context result = expr_context(exprbuf);
            struct parse_context context = parse_context(input, &result);

            if (parse_regex(&context)) {
                struct expr *expr = gexpr(&result);

                if (args.output == OUTPUT_TREE) {
                    printf("expr type: %d\n", expr->type);
                    printf("constructed %ld expressions\n", result.exprbuf - exprbuf);
                    print_expr(expr);
                } else {
                    print_dot(stdout, expr, NULL, TOGRAPHFN regex_to_graph);
                }
            } else {
                print_parse_error(parse_error(&context));
                return EXIT_FAILURE;
            }
        } else if (args.cmd == NFA || args.cmd == NFA_TABLE || args.cmd == NFA_DOT) {
            struct nfa_state statebuf[STATE_MAX];
            struct nfa_context context = nfa_context(statebuf);
            struct nfa_context *nfa = &context;

            nfa_regex(input, nfa);

            if (!has_nfa_error(nfa)) {
                struct nfa mach = gmachine(nfa);

                if (args.cmd == NFA_TABLE) {
                    printf("start state: %p, end state: %p\n", mach.start, *mach.end);
                    print_state_table(statebuf, context.statebuf);
                } else if (args.cmd == NFA_DOT) {
                    nfa_to_graph(statebuf, mach.start);
                } else {
                    printf("nfa success! made %ld states\n", context.statebuf - statebuf);
                }
            } else {
                print_nfa_error(nfa_error(&context));
                return EXIT_FAILURE;
            }
        }
    }

    return EXIT_SUCCESS;
}
