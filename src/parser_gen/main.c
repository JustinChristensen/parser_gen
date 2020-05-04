#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <base/args.h>
#include <base/bitset.h>
#include <base/string.h>
#include <gram/analyze.h>
#include <gram/ll.h>
#include <gram/lr.h>
#include <gram/states.h>
#include <gram/parser.h>

enum command_key {
    GEN_PARSER,
    ANALYZE,
    SCAN,
    PARSE,
    AUTOMATA
};

enum arg_key {
    PARSER_TYPE,
    SPEC_FILE,
    TABLES
};

enum parser_type {
    LL,
    SLR,
    LR1
};

struct args {
    enum command_key cmd;
    enum parser_type type;
    bool tables;
    char spec[BUFSIZ];
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;

    while ((key = readarg(context)) != END) {
        if (cmd == GEN_PARSER) {
            switch (key) {
                case SPEC_FILE:
                    strcpy(args->spec, argval());
                    break;
                case TABLES:
                    args->tables = true;
                    break;
                default: break;
            }
        }

        if (cmd == GEN_PARSER || cmd == AUTOMATA) {
            switch (key) {
                case PARSER_TYPE:
                    if (streq("ll", argval())) {
                        args->type = LL;
                    } else if (streq("slr", argval()) || streq("lr0", argval())) {
                        args->type = SLR;
                    } else if (streq("lr1", argval())) {
                        args->type = LR1;
                    } else {
                        print_usage(stderr, context);
                        exit(EXIT_FAILURE);
                    }
                    break;
                default: break;
            }
        }
    }

    args->cmd = cmd;
    args->pos = argv(context);
    args->posc = argc(context);
}

static size_t slurp_file(int bufsize, char *buf, char *filename) {
    FILE *fi = fopen(filename, "r");

    if (!fi) {
        fprintf(stderr, "failed to open %s\n", filename);
        return -1;
    }

    int nread = fread(buf, sizeof *buf, bufsize, fi);
    buf[nread] = '\0';

    if (ferror(fi)) {
        fprintf(stderr, "failed reading %s\n", filename);
        fclose(fi);
        return -1;
    }

    fclose(fi);

    return nread;
}

int gen_parser(struct args args) {
    int const bufsize = BUFSIZ * 32;
    char specfile[bufsize] = "";
    char **files = args.pos;
    char contents[bufsize] = "";

    if (!args.spec[0]) {
        fprintf(stderr, "spec file required\n");
        return EXIT_FAILURE;
    }

    if (args.posc == 0) {
        fprintf(stderr, "no input files\n");
        return EXIT_FAILURE;
    }

    int nread = 0;
    if ((nread = slurp_file(bufsize, specfile, args.spec)) == -1) {
        fprintf(stderr, "reading spec file %s failed\n", args.spec);
        return EXIT_FAILURE;
    }

    struct gram_spec_parser spec_parser = { 0 };
    struct gram_parse_error parserr = { 0 };
    struct gram_parser_spec spec = { 0 };

    if (!gram_spec_parser(&parserr, &spec_parser) || !gram_parse(&parserr, &spec, specfile, &spec_parser)) {
        print_gram_parse_error(stderr, parserr);
        free_gram_spec_parser(&spec_parser);
        return EXIT_FAILURE;
    }

    free_gram_spec_parser(&spec_parser);

    if (args.type == LL) {
        struct ll_parser parser = { 0 };
        struct ll_error generr = { 0 };

        if (!gen_ll(&generr, &parser, &spec)) {
            print_ll_error(stderr, generr);
            free_gram_parser_spec(&spec);
            return EXIT_FAILURE;
        }

        free_gram_parser_spec(&spec);

        if (args.tables) {
            print_ll_parser(stdout, &parser);
            free_ll_parser(&parser);
            return EXIT_SUCCESS;
        }

        struct ll_parser_state pstate = ll_parser_state(&parser);

        for (int i = 0; i < args.posc; i++) {
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1) {
                fprintf(stderr, "reading file %s failed\n", files[i]);
                free_ll_parser(&parser);
                free_ll_parser_state(&pstate);
                return EXIT_FAILURE;
            }

            if (!ll_parse(&generr, contents, &pstate)) {
                print_ll_error(stderr, generr);
                free_ll_parser(&parser);
                free_ll_parser_state(&pstate);
                return EXIT_FAILURE;
            }

            printf("parsed %s\n", files[i]);
        }

        free_ll_parser(&parser);
        free_ll_parser_state(&pstate);
    } else if (args.type == SLR || args.type == LR1) {
        struct lr_parser parser = { 0 };
        struct lr_error generr = { 0 };

        if (!gen_lr(&generr, &parser, args.type == SLR ? slr_table : lr1_table, &spec)) {
            print_lr_error(stderr, generr);
            free_gram_parser_spec(&spec);
            return EXIT_FAILURE;
        }

        free_gram_parser_spec(&spec);

        if (args.tables) {
            print_lr_parser(stdout, &parser);
            free_lr_parser(&parser);
            return EXIT_SUCCESS;
        }

        struct lr_parser_state pstate = lr_parser_state(&parser);

        for (int i = 0; i < args.posc; i++) {
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1) {
                fprintf(stderr, "reading file %s failed\n", files[i]);
                free_lr_parser(&parser);
                free_lr_parser_state(&pstate);
                return EXIT_FAILURE;
            }

            if (!lr_parse(&generr, contents, &pstate)) {
                print_lr_error(stderr, generr);
                free_lr_parser(&parser);
                free_lr_parser_state(&pstate);
                return EXIT_FAILURE;
            }

            printf("parsed %s\n", files[i]);
        }

        free_lr_parser(&parser);
        free_lr_parser_state(&pstate);
    }

    return EXIT_SUCCESS;
}

int automata(struct args args) {
    if (!args.posc)
        return fprintf(stderr, "no spec file\n"), EXIT_FAILURE;

    if (args.type == LL)
        return fprintf(stderr, "no state machine available\n"), EXIT_FAILURE;

    char *specfile = args.pos[0];
    int const bufsize = BUFSIZ * 32;
    char spec_contents[bufsize] = "";
    int nread = 0;
    if ((nread = slurp_file(bufsize, spec_contents, specfile)) == -1)
        return fprintf(stderr, "reading spec %s failed", specfile), EXIT_FAILURE;

    struct gram_spec_parser spec_parser = { 0 };
    struct gram_parse_error spec_parse_error = { 0 };
    struct gram_parser_spec spec = { 0 };

    if (gram_spec_parser(&spec_parse_error, &spec_parser) && gram_parse(&spec_parse_error, &spec, spec_contents, &spec_parser)) {
        free_gram_spec_parser(&spec_parser);

        struct gram_symbol_analysis san = { 0 };
        if (!gram_analyze_symbols(&san, &spec)) {
            free_gram_parser_spec(&spec);
            return EXIT_FAILURE;
        }

        unsigned nstates;
        enum lr_item_type type = args.type == LR1 ? GM_LR1_ITEMS : GM_LR0_ITEMS;
        struct lr_state *states = discover_lr_states(&nstates, type, &san, &spec);
        free_gram_symbol_analysis(&san);
        free_gram_parser_spec(&spec);
        int result = print_lr_states_dot(stdout, nstates, states);
        // print_lr_states(stdout, nstates, states);
        // int result = 0;
        free_lr_states(nstates, states);
        return result;
    }

    free_gram_spec_parser(&spec_parser);
    print_gram_parse_error(stderr, spec_parse_error);

    return EXIT_FAILURE;
}

int analyze(struct args args, char *contents) {
    struct gram_spec_parser spec_parser = { 0 };
    struct gram_parse_error error = { 0 };
    struct gram_parser_spec spec = { 0 };

    if (gram_spec_parser(&error, &spec_parser) && gram_parse(&error, &spec, contents, &spec_parser)) {
        free_gram_spec_parser(&spec_parser);
        print_gram_parser_spec(stdout, &spec);

        if (args.cmd == ANALYZE) {
            print_gram_stats(stdout, spec.stats);

            struct gram_symbol_analysis san = { 0 };
            if (gram_analyze_symbols(&san, &spec)) {
                print_gram_symbol_analysis(stdout, &san);

                struct gram_rule_analysis ran = { 0 };
                if (gram_analyze_rules(&ran, &san, &spec)) {
                    print_gram_rule_analysis(stdout, &ran);
                    free_gram_rule_analysis(&ran);
                }

                struct gram_analysis gan = { 0 };
                if (gram_analyze(&gan, &san, &spec)) {
                    print_gram_analysis(stdout, &gan);
                    free_gram_analysis(&gan);
                }

                free_gram_symbol_analysis(&san);
            }
        }

        free_gram_parser_spec(&spec);

        return EXIT_SUCCESS;
    }

    print_gram_parse_error(stderr, error);
    free_gram_spec_parser(&spec_parser);
    free_gram_parser_spec(&spec);

    return EXIT_FAILURE;
}

int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = GEN_PARSER,
        .type = LL,
        .spec = "",
        .tables = false
    };

    struct arg parser_type_arg = { PARSER_TYPE, "type", 0, required_argument, "Parser type: ll, slr, lr0, lr1" };
    struct arg spec_file_arg = { SPEC_FILE, "spec", 0, required_argument, "Spec file" };
    struct arg tables_arg = { TABLES, NULL, 'T', no_argument, "Print action table as CSV" };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        GEN_PARSER,
        NULL,
        ARGS { parser_type_arg, spec_file_arg, tables_arg, help_and_version_args, END_ARGS },
        NULL,
        CMDS {
            { ANALYZE, "analyze", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Analyze spec files" },
            { AUTOMATA, "automata", ARGS { parser_type_arg, help_and_version_args, END_ARGS }, NULL, NULL, "Print the LR(0) automaton in dot format " },
            { PARSE, "parse", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Parse spec files" },
            { SCAN, "scan", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Scan spec files" },
            END_CMDS
        },
        "Generate a parser"
    });

    int const bufsize = BUFSIZ * 32;
    char contents[bufsize] = "";
    char **files = args.pos;

    if (args.cmd == GEN_PARSER) {
        return gen_parser(args);
    } else if (args.cmd == AUTOMATA) {
        return automata(args);
    } else {
        for (int i = 0; i < args.posc; i++) {
            int nread = 0;
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1)
                return EXIT_FAILURE;

            printf("filename: %s, size: %d\n", files[i], nread);

            if (args.cmd == SCAN) {
                print_gram_tokens(stdout, contents);
            } else if (!analyze(args, contents))
                return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}
