#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>
#include <gram/echo.h>
#include <gram/pack.h>
#include <gram/parser.h>

enum command_key {
    GEN_PARSER,
    SCAN,
    PARSE,
    PACK
};

enum arg_key {
    STATS
};

struct args {
    enum command_key cmd;
    bool stats;
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END) {
        switch (key) {
            case STATS:
                args->stats = true;
                break;
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
        .stats = false
    };

    struct arg stats_arg = { STATS, "stats", 0, no_argument, "Print stats" };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        GEN_PARSER,
        NULL,
        ARGS { help_and_version_args, END_ARGS },
        NULL,
        CMDS {
            { SCAN, "scan", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Scan spec files" },
            { PARSE, "parse", ARGS { stats_arg, help_and_version_args, END_ARGS }, NULL, NULL, "Parse spec files" },
            { PACK, "pack", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Pack grammar from spec files" },
            END_CMDS
        },
        "Generate a parser"
    });

    if (args.cmd == GEN_PARSER) {
    } else {
        char **files = args.pos;
        int const bufsize = BUFSIZ * 32;
        char contents[bufsize] = "";

        for (int i = 0; i < args.posc; i++) {
            int nread = 0;
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1)
                return EXIT_FAILURE;

            printf("filename: %s, size: %d\n", files[i], nread);

            if (args.cmd == SCAN) {
                print_gram_tokens(stdout, contents);
            } else if (args.cmd == PARSE) {
                struct gram_parse_context context = { 0 };

                if (gram_parse_context(&context) && parse_gram_parser_spec(contents, &context)) {
                    struct gram_parser_spec *pspec = gram_parser_spec(&context);
                    if (args.stats) echo_gram_pspec_stats(stderr, pspec);
                    echo_gram_pspec(stdout, pspec);
                    free_gram_parse_context(&context);
                } else {
                    print_gram_parse_error(stderr, gram_parser_error(&context));
                    free_gram_parse_context(&context);
                    return EXIT_FAILURE;
                }
            } else if (args.cmd == PACK) {
                struct gram_parse_context context = { 0 };

                if (gram_parse_context(&context) && parse_gram_parser_spec(contents, &context)) {
                    struct gram_parser_spec *spec = gram_parser_spec(&context);
                    struct hash_table *symtab = gram_symtab(&context);
                    struct gram_stats stats = gram_stats(&context);
                    struct gram_packed_spec *pspec = gram_pack(spec, symtab, stats);
                    if (!pspec) {
                        fprintf(stderr, "packing failed\n");
                        free_gram_parse_context(&context);
                        return EXIT_FAILURE;
                    }
                    print_gram_packed_spec(stdout, pspec);
                    free_gram_packed_spec(pspec);
                    free_gram_parse_context(&context);
                } else {
                    print_gram_parse_error(stderr, gram_parser_error(&context));
                    free_gram_parse_context(&context);
                    return EXIT_FAILURE;
                }
            }
        }
    }

    return EXIT_SUCCESS;
}
