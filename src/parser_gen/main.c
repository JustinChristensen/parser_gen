#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>
#include <base/bitset.h>
#include <base/string.h>
#include <gram/analyze.h>
#include <gram/ll1.h>
#include <gram/parser.h>

enum command_key {
    GEN_PARSER,
    ANALYZE,
    SCAN,
    PARSE
};

enum arg_key {
    PARSER_TYPE
};

enum parser_type {
    LL1
};

struct args {
    enum command_key cmd;
    enum parser_type type;
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;

    while ((key = readarg(context)) != END) {
        if (cmd == GEN_PARSER) {
            if (streq("ll1", argval())) {
                args->type = LL1;
            } else {
                print_usage(stderr, context);
                exit(EXIT_FAILURE);
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

int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = GEN_PARSER,
        .type = LL1
    };

    struct arg parser_type_arg = { PARSER_TYPE, "type", 0, optional_argument, "Parser type: ll1" };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        GEN_PARSER,
        NULL,
        ARGS { parser_type_arg, help_and_version_args, END_ARGS },
        NULL,
        CMDS {
            { ANALYZE, "analyze", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Analyze spec files" },
            { SCAN, "scan", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Scan spec files" },
            { PARSE, "parse", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Parse spec files" },
            END_CMDS
        },
        "Generate a parser"
    });

    int const bufsize = BUFSIZ * 32;
    char contents[bufsize] = "";
    char **files = args.pos;

    if (args.cmd == GEN_PARSER) {
        if (args.posc == 0) {
            fprintf(stderr, "no input files\n");
            return EXIT_FAILURE;
        }

        int nread = 0;
        if ((nread = slurp_file(bufsize, contents, files[0])) == -1)
            return EXIT_FAILURE;

        struct gram_parse_context context = { 0 };
        struct gram_parse_error parserr = { 0 };

        if (gram_parse_context(&parserr, &context)) {
            struct gram_parser_spec spec = { 0 };
            if (gram_parse(&parserr, &spec, contents, &context)) {
                free_gram_parse_context(&context);

                struct ll1_parser parser = { 0 };
                struct ll1_error generr = { 0 };

                if (!gen_ll1(&generr, &parser, &spec)) {
                    print_ll1_error(stderr, generr);
                    free_gram_parser_spec(&spec);
                    return EXIT_FAILURE;
                }

                print_ll1_parser(stdout, &parser);

                free_ll1_parser(&parser);
                free_gram_parser_spec(&spec);

                return EXIT_SUCCESS;
            }
        }
    } else {
        for (int i = 0; i < args.posc; i++) {
            int nread = 0;
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1)
                return EXIT_FAILURE;

            printf("filename: %s, size: %d\n", files[i], nread);

            if (args.cmd == SCAN) {
                print_gram_tokens(stdout, contents);
            } else {
                struct gram_parse_context context = { 0 };
                struct gram_parse_error error = { 0 };

                if (gram_parse_context(&error, &context)) {
                    struct gram_parser_spec spec = { 0 };
                    if (gram_parse(&error, &spec, contents, &context)) {
                        free_gram_parse_context(&context);
                        print_gram_parser_spec(stdout, &spec);

                        if (args.cmd == ANALYZE) {
                            print_gram_stats(stdout, spec.stats);
                            bool *nullable = gram_nullable(&spec);
                            struct bitset **firsts = gram_firsts(nullable, &spec);
                            struct bitset **follows = gram_follows(nullable, firsts, &spec);
                            print_gram_nullable(stdout, nullable, &spec);
                            printf("firsts:\n");
                            print_gram_sets(stdout, firsts, spec.stats);
                            printf("follows:\n");
                            print_gram_sets(stdout, follows, spec.stats);
                            free(nullable);
                            free_gram_sets(firsts, spec.stats);
                            free_gram_sets(follows, spec.stats);
                        }

                        free_gram_parser_spec(&spec);
                        return EXIT_SUCCESS;
                    }
                }

                print_gram_parse_error(stderr, error);
                free_gram_parse_context(&context);
                return EXIT_FAILURE;
            }
        }
    }

    return EXIT_SUCCESS;
}
