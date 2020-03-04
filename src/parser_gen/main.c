#include <stdio.h>
#include <stdlib.h>
#include <base/args.h>
#include <gram/echo.h>
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
            // generate recursive LL parser
            // generate configuration-driven parser:
            //      generate LL parser
            //          lookahead :: Terminal
            //          input :: [Terminal]
            //          select_production :: NonTerminal -> Terminal -> Production
            //          rule_symbols :: Production -> [Symbol]
            //          symbol_stack :: [Symbol]
            //      generate canonical LR parser
            //          lookahead :: Terminal
            //          input :: [Terminal]
            //          stack :: undefined
            //          data Action = Shift | Reduce | Accept | Error
            //          action :: State -> Terminal -> Action
            //      generate simple LR parser       (SLR)
            //      generate lookahead LR parser    (LALR)
            END_CMDS
        },
        "Generate a parser"
    });

    if (args.cmd == GEN_PARSER) {
    } else if (args.cmd == PARSE) {
        char **files = args.pos;
        int const bufsize = BUFSIZ * 32;
        char contents[bufsize] = "";

        for (int i = 0; i < args.posc; i++) {
            int nread = 0;
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1)
                return EXIT_FAILURE;

            printf("filename: %s, size: %d\n", files[i], nread);

            struct gram_parse_context context = { 0 };
            struct gram_ast_context ast_context = { 0 };

            if (gram_ast_context(&ast_context) && gram_parse_context(&context, &ast_context, &gram_ast_iface) &&
                parse_gram_parser_spec(contents, &context)) {
                echo_gram_parser_spec(stdout, gram_parser_spec(&ast_context));
                free_gram_ast_context(&ast_context);
                free_gram_parse_context(&context);
            } else {
                print_gram_error(stderr, gram_parser_error(&context));
                free_gram_ast_context(&ast_context);
                free_gram_parse_context(&context);
                return EXIT_FAILURE;
            }
        }
    } else if (args.cmd == SCAN) {
        char **files = args.pos;
        int const bufsize = BUFSIZ * 32;
        char contents[bufsize] = "";

        for (int i = 0; i < args.posc; i++) {
            int nread = 0;
            if ((nread = slurp_file(bufsize, contents, files[i])) == -1)
                return EXIT_FAILURE;

            printf("filename: %s, size: %d\n", files[i], nread);
            print_gram_tokens(stdout, contents);
        }
    }

    return EXIT_SUCCESS;
}
