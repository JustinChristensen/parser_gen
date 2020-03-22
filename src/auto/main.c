#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <base/args.h>
#include <base/debug.h>
#include <base/graphviz.h>
#include <regex/ast.h>
#include <regex/nfa.h>
#include <regex/parser.h>
#include "dot.h"
#include "print_ast.h"

enum command_key {
    AUTO,
    PRINT,
    NFA,
    SCAN_ONLY,
    C_FILE
};

enum arg_key {
    FORMAT,
    REGEX
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
    int posc;
    char **pos;
};

static char *cmd_str(enum command_key key) {
    switch (key) {
        case AUTO:      return "AUTO";
        case PRINT:     return "PRINT";
        case NFA:       return "NFA";
        case SCAN_ONLY: return "SCAN_ONLY";
        case C_FILE:    return "C_FILE";
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
                    case C_FILE:
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

#define debug(...) debug_ns("args", __VA_ARGS__)
static void debug_args(struct args args) {
    debug("cmd: %s\n", cmd_str(args.cmd));
    debug("output: %s\n", output_str(args.output));
    if (args.regex) debug("regex: %s\n", args.regex);
    if (args.posc > 0) {
        debug("posc: %d\n", args.posc);
        debug("pos: ");
        for (int i = 0; i < args.posc; i++) {
            debug("%s ", args.pos[i]);
        }
        debug("\n");
    }
}

enum cfile_symbol {
    C_ERROR,

    C_INCLUDE,
    C_DEFINE,
    C_UNDEF,
    C_LINE_COMMENT,
    C_IF,
    C_ELSE,
    C_SWITCH,
    C_CASE,
    C_BREAK,
    C_CONTINUE,
    C_WHILE,
    C_DO,
    C_RETURN,
    C_INT,
    C_CHAR,
    C_BOOL,
    C_ENUM,
    C_STRUCT,
    C_UNION,
    C_LPAREN,
    C_RPAREN,
    C_LBRACKET,
    C_RBRACKET,
    C_SEMI,
    C_COMMA,
    C_LBRACE,
    C_RBRACE,
    C_STAR,
    C_AMP,
    C_QUESTION,
    C_COLON,
    C_DOT,
    C_ARROW,
    C_PLUS,
    C_MINUS,
    C_SLASH,
    C_AND,
    C_OR,
    C_LT,
    C_LTEQ,
    C_GT,
    C_GTEQ,
    C_EQ,
    C_NEQ,
    C_ASSIGN,
    C_NEGATE,
    C_INCREMENT,
    C_DECREMENT,
    C_CHAR_LIT,
    C_STRING_LIT,
    C_INT_LIT,
    C_NULL_LIT,
    C_BOOL_LIT,
    C_CONSTANT,
    C_IDENTIFIER,
    C_WS
};

static int const NUM_CSYMS = C_WS + 1;

static char *str_for_csym(enum cfile_symbol sym) {
    switch (sym) {
        case C_ERROR:        return "ERROR";
        case C_INCLUDE:      return "#include";
        case C_DEFINE:       return "#define";
        case C_UNDEF:        return "#undef";
        case C_LINE_COMMENT: return "//...";
        case C_IF:           return "if";
        case C_ELSE:         return "else";
        case C_SWITCH:       return "switch";
        case C_CASE:         return "case";
        case C_BREAK:        return "break";
        case C_CONTINUE:     return "continue";
        case C_WHILE:        return "while";
        case C_DO:           return "do";
        case C_RETURN:       return "return";
        case C_INT:          return "int";
        case C_CHAR:         return "char";
        case C_BOOL:         return "bool";
        case C_ENUM:         return "enum";
        case C_STRUCT:       return "struct";
        case C_UNION:        return "union";
        case C_LPAREN:       return "(";
        case C_RPAREN:       return ")";
        case C_LBRACKET:     return "[";
        case C_RBRACKET:     return "]";
        case C_SEMI:         return ";";
        case C_COMMA:        return ",";
        case C_LBRACE:       return "{";
        case C_RBRACE:       return "}";
        case C_STAR:         return "*";
        case C_AMP:          return "&";
        case C_QUESTION:     return "?";
        case C_COLON:        return ":";
        case C_DOT:          return ".";
        case C_ARROW:        return "->";
        case C_PLUS:         return "+";
        case C_MINUS:        return "-";
        case C_SLASH:        return "/";
        case C_AND:          return "&&";
        case C_OR:           return "||";
        case C_LT:           return "<";
        case C_LTEQ:         return "<=";
        case C_GT:           return ">";
        case C_GTEQ:         return ">=";
        case C_EQ:           return "==";
        case C_NEQ:          return "!=";
        case C_ASSIGN:       return "=";
        case C_NEGATE:       return "!";
        case C_INCREMENT:    return "++";
        case C_DECREMENT:    return "--";
        case C_CHAR_LIT:     return "'a'";
        case C_STRING_LIT:   return "\"abc\"";
        case C_INT_LIT:      return "9000";
        case C_BOOL_LIT:     return "true";
        case C_NULL_LIT:     return "NULL";
        case C_CONSTANT:     return "CONSTANT";
        case C_IDENTIFIER:   return "identifier";
        case C_WS:           return "whitespace";
    }
};

static void print_sym_counts(int *symcount) {
    printf("%3s\t%14s\t%s\n", "sym", "", "count");

    for (int i = 0; i < NUM_CSYMS; i++) {
        printf("%3d\t%14s\t%d\n", i, str_for_csym(i), symcount[i]);
    }
}

#define BUFFER_SIZE 4096
int main(int argc, char *argv[]) {
    struct args args = {
        .cmd = AUTO,
        .output = OUTPUT_TRIAL,
        .regex = NULL,
        .posc = 0,
        .pos = NULL
    };

    struct arg print_fmt_arg = { FORMAT, "format", 'f', required_argument, "Output format: dot, table, or tree" };
    struct arg dot_fmt_arg = { FORMAT, "format", 'f', required_argument, "Output format: dot" };
    struct arg regex_arg = { REGEX, NULL, 'r', required_argument, "Regular expression" };

    struct env_var debug_var = { "DEBUG", "Print debug output" };
    struct env_var nonrec_var = { "USE_NONREC", "Use the non-recursive parser" };

    run_args(&args, ARG_FN read_args, "1.0.0", argc, argv, NULL, CMD {
        AUTO,
        NULL,
        ARGS { help_and_version_args, END_ARGS },
        ENV_VARS { debug_var, END_ENV_VARS },
        CMDS {
            {
                PRINT, "print",
                ARGS { print_fmt_arg, help_and_version_args, regex_arg, END_ARGS },
                ENV_VARS { debug_var, nonrec_var, END_ENV_VARS },
                NULL,
                "Print the syntax tree for each regular expression"
            },
            {
                NFA, "nfa",
                ARGS { dot_fmt_arg, help_and_version_args, regex_arg, END_ARGS },
                ENV_VARS { debug_var, nonrec_var, END_ENV_VARS },
                NULL,
                "Construct and simulate an NFA"
            },
            {
                SCAN_ONLY, "scan",
                ARGS { help_and_version_args, regex_arg, END_ARGS },
                ENV_VARS { debug_var, END_ENV_VARS },
                NULL,
                "Run the scanner standalone"
            },
            {
                C_FILE, "cfile",
                ARGS { dot_fmt_arg, help_and_version_args, END_ARGS },
                ENV_VARS { debug_var, nonrec_var, END_ENV_VARS },
                NULL,
                "Scan a C file"
            },
            END_CMDS
        },
        "Construct and simulate automata"
    });

    debug_args(args);

    if (args.cmd == PRINT) {
        struct regex_expr exprbuf[RX_EXPR_MAX];
        struct regex_expr_context econtext = expr_context(exprbuf);
        char *regex = args.regex ? args.regex : "(a|b)*abbc?";
        struct regex_parse_context pcontext = regex_parse_context(&econtext, expr_parse_iface);

        if (parse_regex(regex, &pcontext)) {
            struct regex_expr *expr = gexpr(&econtext);

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
            print_regex_error(stderr, regex_parse_error(&pcontext));
            return EXIT_FAILURE;
        }

        free_expr_context(&econtext);
    } else if (args.cmd == NFA) {
        struct nfa_context ncontext;
        bool success;

        if (!nfa_context(&ncontext, NULL)) {
            fprintf(stderr, "could not allocate an nfa context\n");
            return EXIT_FAILURE;
        }

        if (args.regex) {
            success = nfa_regex(35, NULL, args.regex, &ncontext);
        } else {
            success =
                nfa_regex(RX_TAG_ONLY, "alpha", "[A-Za-z_]", &ncontext) &&
                nfa_regex(RX_TAG_ONLY, "alnum", "[0-9A-Za-z_]", &ncontext) &&
                nfa_regex(0, "if", "if", &ncontext) &&
                nfa_regex(1, "else", "else", &ncontext) &&
                nfa_regex(2, "for", "for", &ncontext) &&
                nfa_regex(3, "while", "while", &ncontext) &&
                nfa_regex(4, "do", "do", &ncontext) &&
                nfa_regex(5, NULL, "[ \t\n]", &ncontext) &&
                nfa_regex(6, NULL, "{alpha}{alnum}*", &ncontext);
        }

        if (!success) {
            print_regex_error(stderr, nfa_error(&ncontext));
            free_nfa_context(&ncontext);
            return EXIT_FAILURE;
        }

        if (!nfa_has_error(&ncontext)) {
            if (args.output == OUTPUT_DOT) {
                struct nfa mach = nfa_gmachine(&ncontext);
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

                    if (nfa_start_match(buf, &match, &ncontext)) {
                        int sym = 0;

                        while ((sym = nfa_match(&match)) != RX_EOF) {
                            if (sym == RX_REJECTED) {
                                printf("rejected input\n");
                                break;
                            }

                            nfa_match_lexeme(matchbuf, &match);
                            printf("%s at ", matchbuf);
                            print_regex_loc(stdout, nfa_match_loc(&match));
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
            print_regex_error(stderr, nfa_error(&ncontext));
            return EXIT_FAILURE;
        }

        free_nfa_context(&ncontext);
    } else if (args.cmd == SCAN_ONLY && args.regex) {
        print_regex_token_table(args.regex);
    } else if (args.cmd == C_FILE) {
        struct nfa_context context;

        if (!nfa_context(&context, RX_PATTERNS {
            RX_ALPHA_(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
            { C_INCLUDE, NULL, "#include *(\"[^\"]*\"|<[^>]*>)\n" },
            { C_DEFINE, NULL, "#define *[^\n]*\n" },
            { C_UNDEF, NULL, "#undef *[^\n]*\n" },
            RX_LINE_COMMENT(C_LINE_COMMENT),
            { C_IF, NULL, "if" },
            { C_ELSE, NULL, "else" },
            { C_SWITCH, NULL, "switch" },
            { C_CASE, NULL, "case" },
            { C_BREAK, NULL, "break" },
            { C_CONTINUE, NULL, "continue" },
            { C_WHILE, NULL, "while" },
            { C_DO, NULL, "do" },
            { C_RETURN, NULL, "return" },
            { C_INT, NULL, "int" },
            { C_CHAR, NULL, "char" },
            { C_BOOL, NULL, "bool" },
            { C_ENUM, NULL, "enum" },
            { C_STRUCT, NULL, "struct" },
            { C_UNION, NULL, "union" },
            { C_LPAREN, NULL, "\\(" },
            { C_RPAREN, NULL, "\\)" },
            { C_LBRACKET, NULL, "\\[" },
            { C_RBRACKET, NULL, "\\]" },
            { C_SEMI, NULL, ";" },
            { C_COMMA, NULL, "," },
            { C_LBRACE, NULL, "\\{" },
            { C_RBRACE, NULL, "\\}" },
            { C_STAR, NULL, "\\*" },
            { C_AMP, NULL, "&" },
            { C_QUESTION, NULL, "\\?" },
            { C_COLON, NULL, ":" },
            { C_DOT, NULL, "\\." },
            { C_ARROW, NULL, "->" },
            { C_PLUS, NULL, "\\+" },
            { C_MINUS, NULL, "\\-" },
            { C_SLASH, NULL, "/" },
            { C_AND, NULL, "&&" },
            { C_OR, NULL, "\\|\\|" },
            { C_LT, NULL, "<" },
            { C_LTEQ, NULL, "<=" },
            { C_GT, NULL, ">" },
            { C_GTEQ, NULL, ">=" },
            { C_EQ, NULL, "==" },
            { C_NEQ, NULL, "!=" },
            { C_ASSIGN, NULL, "=" },
            { C_NEGATE, NULL, "!" },
            { C_INCREMENT, NULL, "\\+\\+" },
            { C_DECREMENT, NULL, "--" },
            { C_CHAR_LIT, NULL, "'(\\\\.|[^'\\\\])*'" },
            { C_STRING_LIT, NULL, "\"(\\\\.|[^\"\\\\])*\"" },
            { C_INT_LIT, NULL, "[0-9]+" },
            { C_BOOL_LIT, NULL, "(true|false)" },
            { C_NULL_LIT, NULL, "NULL" },
            { C_CONSTANT, NULL, "[A-Z_]+" },
            { C_IDENTIFIER, NULL, "{alpha_}{alnum_}*" },
            { RX_SKIP, NULL, "[ \t\n]+" },
            RX_END_PATTERNS
        })) {
            fprintf(stderr, "could not initialize context\n");
            print_regex_error(stderr, nfa_error(&context));
            free_nfa_context(&context);
            return EXIT_FAILURE;
        }

        if (args.output == OUTPUT_DOT) {
            nfa_to_graph(nfa_gmachine(&context).start, context.num_states);
            free_nfa_context(&context);
        } else {
            if (args.posc == 0) {
                fprintf(stderr, "no input files\n");
                free_nfa_context(&context);
                return EXIT_FAILURE;
            }

            bool result = EXIT_SUCCESS;
            struct nfa_match match = {0};
            char **files = args.pos;
            FILE *fi;
            static int const bufsize = BUFSIZ * 32;
            char input[bufsize] = "";

            for (int i = 0; i < args.posc; i++) {
                int nread = 0;

                if (!(fi = fopen(files[i], "r"))) {
                    fprintf(stderr, "failed to open file %s\n", files[i]);
                    result = EXIT_FAILURE;
                    break;
                }

                nread = fread(input, sizeof *input, bufsize, fi);
                if (ferror(fi)) {
                    fprintf(stderr, "reading file %s failed\n", files[i]);
                    fclose(fi);
                    result = EXIT_FAILURE;
                    break;
                }

                input[nread] = '\0';

                if (!nfa_start_match(input, &match, &context)) {
                    fprintf(stderr, "could not initialize scanner\n");
                    fclose(fi);
                    result = EXIT_FAILURE;
                    break;
                }

                printf("filename: %s, size: %d\n", files[i], nread);
                printf("%-3s\t%-4s\t%-3s\t%s\n", "sym", "line", "col", "lexeme");

                int sym;
                struct regex_loc loc;
                char lexeme[BUFSIZ] = "";
                int nrejected = 0;

                int symcount[NUM_CSYMS] = { 0 };

                while ((sym = nfa_match(&match)) != RX_EOF) {
                    symcount[sym]++;
                    if (sym == RX_REJECTED) nrejected++;

                    nfa_match_lexeme(lexeme, &match);
                    loc = nfa_match_loc(&match);
                    printf("%3d\t%4d\t%3d\t%s\n", sym, loc.line, loc.col, lexeme);
                }

                if (nrejected) {
                    printf("\n# rejected: %d\n", nrejected);
                    result = EXIT_FAILURE;
                    fclose(fi);
                    break;
                }

                print_sym_counts(symcount);

                fclose(fi);
            }

            free_nfa_match(&match);
            free_nfa_context(&context);

            return result;
        }
    }

    return EXIT_SUCCESS;
}
