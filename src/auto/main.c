#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <base/args.h>
#include <base/debug.h>
#include <base/graphviz.h>
#include <regex/ast.h>
#include <regex/nfa.h>
#include <regex/run_parser.h>
#include "dot.h"
#include "print_ast.h"

enum command_key {
    AUTO,
    PRINT,
    NFA,
    SCAN_ONLY
};

enum arg_key {
    FORMAT,
    REGEX,
    NONREC
};

enum output_fmt {
    OUTPUT_TRIAL,
    OUTPUT_TREE,
    OUTPUT_TABLE,
    OUTPUT_DOT
};

struct args {
    enum command_key cmd;
    enum output_fmt output;
    char *regex;
    bool nonrec;
    int posc;
    char **pos;
};

static char *cmd_str(enum command_key key) {
    switch (key) {
        case AUTO:      return "AUTO";
        case PRINT:     return "PRINT";
        case NFA:       return "NFA";
        case SCAN_ONLY: return "SCAN_ONLY";
    }

    return "";
}

static char *output_str(enum output_fmt fmt) {
    switch (fmt) {
        case OUTPUT_TRIAL: return "OUTPUT_TRIAL";
        case OUTPUT_TREE: return "OUTPUT_TREE";
        case OUTPUT_TABLE: return "OUTPUT_TABLE";
        case OUTPUT_DOT: return "OUTPUT_DOT";
    }

    return "";
}

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END) {
        switch (key) {
            case NONREC:
                args->nonrec = true;
                break;
            case FORMAT:
                switch (cmd) {
                    case PRINT:
                        if (strcmp("dot", argval()) == 0) {
                            args->output = OUTPUT_DOT;
                        } else if (strcmp("table", argval()) == 0) {
                            args->output = OUTPUT_TABLE;
                        } else if (strcmp("tree", argval()) == 0) {
                            args->output = OUTPUT_TREE;
                        } else {
                            print_usage(stderr, context);
                            exit(EXIT_FAILURE);
                        }
                        break;
                    case NFA:
                        if (strcmp("dot", argval()) == 0) {
                            args->output = OUTPUT_DOT;
                        } else {
                            print_usage(stderr, context);
                            exit(EXIT_FAILURE);
                        }
                        break;
                }
                break;
            case REGEX:
                args->regex = argval();
                break;
        }
    }
    args->cmd = cmd;
    args->pos = argv(context);
    args->posc = argc(context);
}

#define adebug(...) debug_ns_("args", __VA_ARGS__)
static void debug_args(struct args args) {
    adebug("cmd: %s\n", cmd_str(args.cmd));
    adebug("output: %s\n", output_str(args.output));
    if (args.regex) adebug("regex: %s\n", args.regex);
    adebug("nonrec: %s\n", args.nonrec ? "true" : "false");
    if (args.posc > 0) {
        adebug("posc: %d\n", args.posc);
        adebug("pos: ");
        for (int i = 0; i < args.posc; i++) {
            debug_("%s ", args.pos[i]);
        }
        debug_("\n");
    }
}

#define BUFFER_SIZE 4096
int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = AUTO,
        .output = OUTPUT_TRIAL,
        .regex = NULL,
        .nonrec = false,
        .posc = 0,
        .pos = NULL
    };

    struct arg print_fmt_arg = { FORMAT, "format", 'f', required_argument, "Output format: dot, table, or tree" };
    struct arg nfa_fmt_arg = { FORMAT, "format", 'f', required_argument, "Output format: dot" };
    struct arg regex_arg = { REGEX, NULL, 'r', required_argument, "Regular expression" };
    struct arg parse_nonrec_arg = { NONREC, "nonrec", 0, no_argument, "Use the non-recursive parser instead" };

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
                    print_fmt_arg,
                    parse_nonrec_arg,
                    help_and_version_args,
                    regex_arg,
                    END_ARGS
                },
                NULL,
                "Print the syntax tree for each regular expression"
            },
            {
                NFA,
                "nfa",
                ARGS {
                    nfa_fmt_arg,
                    parse_nonrec_arg,
                    help_and_version_args,
                    regex_arg,
                    END_ARGS
                },
                NULL,
                "Construct and simulate an NFA"
            },
            {
                SCAN_ONLY,
                "scan",
                ARGS {
                    help_and_version_args,
                    regex_arg,
                    END_ARGS
                },
                NULL,
                "Run the scanner standalone"
            },
            END_CMDS
        },
        "Construct and simulate automata"
    });

    debug_args(args);

    if (args.cmd == PRINT) {
        struct expr exprbuf[EXPR_MAX];
        struct expr_context econtext = expr_context(exprbuf);
        char *regex = args.regex ? args.regex : "(a|b)*abbc?";
        struct parse_context pcontext = parse_context(&econtext, expr_pinterface, expr_actions, args.nonrec);

        if (run_parser(regex, &pcontext)) {
            struct expr *expr = gexpr(&econtext);

            if (args.output == OUTPUT_DOT) {
                print_dot(stdout, expr, NULL, TOGRAPHFN regex_to_graph);
            } else if (args.output == OUTPUT_TABLE) {
                printf("constructed %ld expressions\n", econtext.bufp - exprbuf);
                print_expr_table(exprbuf, econtext.bufp);
            } else {
                printf("constructed %ld expressions\n", econtext.bufp - exprbuf);
                print_expr(expr);
            }
        } else {
            print_regex_error(parse_error(&pcontext));
            return EXIT_FAILURE;
        }

        free_expr_context(&econtext);
    } else if (args.cmd == NFA) {
        struct nfa_context ncontext;

        if (!nfa_context(NULL, args.nonrec, &ncontext)) {
            fprintf(stderr, "could not allocate an nfa context\n");
            return EXIT_FAILURE;
        }

        bool success = true;
        if (args.regex) {
            success = nfa_regex(35, NULL, args.regex, &ncontext);
        } else {
            success =
                nfa_regex(1, "if", "if", &ncontext) &&
                nfa_regex(2, "else", "else", &ncontext) &&
                nfa_regex(3, "for", "for", &ncontext) &&
                nfa_regex(4, "while", "while", &ncontext) &&
                nfa_regex(5, "do", "do", &ncontext) &&
                nfa_regex(6, NULL, "[ \n]", &ncontext);
                nfa_regex(7, NULL, "a*", &ncontext);
        }

        if (!success) {
            print_regex_error(nfa_error(&ncontext));
            return EXIT_FAILURE;
        }

        if (!nfa_has_error(&ncontext)) {
            if (args.output == OUTPUT_DOT) {
                struct nfa mach = gmachine(&ncontext);
                nfa_to_graph(mach.start, ncontext.num_states);
            } else {
                FILE *in = NULL;

                if (args.posc == 0) {
                    if (!isatty(STDIN_FILENO)) {
                        in = stdin;
                    }
                } else {
                    in = fopen(args.pos[0], "r");
                }

                if (in) {
                    char buf[BUFSIZ] = "";
                    char matchbuf[BUFSIZ] = "";

                    int nread = fread(buf, sizeof *buf, BUFSIZ, in);
                    buf[nread] = '\0';

                    struct nfa_match match;

                    if (nfa_match_state(&match, buf, &ncontext)) {
                        int sym = 0;

                        while ((sym = nfa_match(&match))) {
                            nfa_match_lexeme(matchbuf, &match);
                            printf("%s at ", matchbuf);
                            regex_print_loc(stdout, nfa_match_loc(&match));
                            printf(" %s, sym: %d\n", sym ? "matches" : "does not match", sym);
                        }

                        free_nfa_match(&match);
                    }

                    fclose(in);
                } else {
                    fprintf(stderr, "no input file\n");
                    return EXIT_FAILURE;
                }
            }
        } else {
            print_regex_error(nfa_error(&ncontext));
            return EXIT_FAILURE;
        }

        free_nfa_context(&ncontext);
    } else if (args.cmd == SCAN_ONLY && args.regex) {
        print_token_table(args.regex);
    }

    return EXIT_SUCCESS;
}
