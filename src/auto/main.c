#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <base/args.h>
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
    REGEX
};

enum output_fmt {
    OUTPUT_DOT,
    OUTPUT_TREE
};

struct args {
    enum command_key cmd;
    enum output_fmt output;
    bool state_table;
    char *regex;
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
            case NFA:
                if (key == REGEX) {
                    args->regex = argval();
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
        .regex = NULL,
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
                    fmt_arg,
                    help_and_version_args,
                    END_ARGS
                },
                NULL,
                "Print the syntax tree for each regular expression"
            },
            {
                NFA,
                "nfa",
                ARGS {
                    { REGEX, NULL, 'r', required_argument, "Regular expression" },
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

    if (args.cmd != NFA) {
        // if (!isatty(STDIN_FILENO)) {
        //     char input[BUFFER_SIZE] = "";
        //     size_t nread = fread(input, sizeof *input, BUFFER_SIZE, stdin);
        //     input[nread] = '\0';

        //     if (args.cmd == PRINT) {
        //         struct expr exprbuf[EXPR_MAX];
        //         struct expr_context result = expr_context(exprbuf);
        //         struct parse_context context = parse_context(input, &result);

        //         if (parse_regex(&context)) {
        //             struct expr *expr = gexpr(&result);

        //             if (args.output == OUTPUT_TREE) {
        //                 printf("expr type: %d\n", expr->type);
        //                 printf("constructed %ld expressions\n", result.exprbuf - exprbuf);
        //                 print_expr(expr);
        //             } else {
        //                 print_dot(stdout, expr, NULL, TOGRAPHFN regex_to_graph);
        //             }
        //         } else {
        //             print_parse_error(parse_error(&context));
        //             return EXIT_FAILURE;
        //         }
        //  } else if (args.cmd == NFA_TABLE || args.cmd == NFA_DOT) {
            if (args.cmd == NFA_TABLE || args.cmd == NFA_DOT) {
                struct nfa_state statebuf[STATE_MAX];
                struct nfa_context context = nfa_context(statebuf);
                struct nfa_context *nfa = &context;

                nfa_regex("if", nfa);
                nfa_regex("else", nfa);
                nfa_regex("for", nfa);
                nfa_regex("while", nfa);
                nfa_regex("do", nfa);

                if (!has_nfa_error(nfa)) {
                    struct nfa mach = gmachine(nfa);

                    printf("%s\n", nfa_match("while", &context) ? "match" : "no match");

                    if (args.cmd == NFA_TABLE) {
                        printf("start state: %p, end state: %p\n", mach.start, *mach.end);
                        print_state_table(statebuf, context.statebuf);
                    } else if (args.cmd == NFA_DOT) {
                        nfa_to_graph(mach.start);
                    }
                } else {
                    print_nfa_error(nfa_error(&context));
                    return EXIT_FAILURE;
                }
            }
        }
    // }

    if (args.cmd == NFA) {
        FILE *in = NULL;

        if (args.posc == 0) {
            if (!isatty(STDIN_FILENO)) {
                in = stdin;
            }
        } else {
            in = fopen(args.pos[0], "r");
        }

        if (in) {
            struct nfa_state statebuf[STATE_MAX];
            struct nfa_context context = nfa_context(statebuf);
            struct nfa_context *nfa = &context;

            nfa_regex(args.regex, nfa);

            if (!has_nfa_error(nfa)) {
                char buf[BUFSIZ];
                printf("regex: %s\n", args.regex);
                while (fgets(buf, BUFSIZ, in)) {
                    buf[strlen(buf) - 1] = '\0';
                    bool matches = nfa_match(buf, &context);
                    printf("%s %s\n", buf, matches ? "matches" : "does not match");
                }
            } else {
                print_nfa_error(nfa_error(&context));
                return EXIT_FAILURE;
            }

            fclose(in);
        } else {
            fprintf(stderr, "no input file\n");
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}
