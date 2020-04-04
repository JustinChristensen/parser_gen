#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>
#include <gram/parser.h>

enum command_key {
    GEN_PARSER,
    SCAN,
    PARSE
};

// enum arg_key {
// };

struct args {
    enum command_key cmd;
    bool stats;
    int posc;
    char **pos;
};

void read_args(struct args *args, int cmd, struct args_context *context) {
    int key;
    while ((key = readarg(context)) != END);
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
        .cmd = GEN_PARSER
    };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        GEN_PARSER,
        NULL,
        ARGS { help_and_version_args, END_ARGS },
        NULL,
        CMDS {
            { SCAN, "scan", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Scan spec files" },
            { PARSE, "parse", ARGS { help_and_version_args, END_ARGS }, NULL, NULL, "Parse spec files" },
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
                struct gram_parse_error error = { 0 };

                if (gram_parse_context(&error, &context)) {
                    struct gram_parser_spec spec = { 0 };
                    if (gram_parse(&error, &spec, contents, &context)) {
                        print_gram_parser_spec(stdout, &spec);
                        free_gram_parser_spec(&spec);
                        free_gram_parse_context(&context);
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
