#include <stdlib.h>
#include <assert.h>
#include <linked_list.h>
#include <ctype.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include "scanner.h"

struct scan_result *init_scan_result(struct token *token, char *input) {
    struct scan_result *scan_result = malloc(*scan_result);
    assert(scan_result != NULL);
    scan_result->token = token;
    scan_result->input = input;
    return scan_result;
}

struct location empty_loc() {
    return { 0, 0 };
}

void *free_scan_result(struct scan_result *scan_result) {
    free_token(scan_result->token);
    scan_result->token = NULL;
    free(scan_result);
}

char *spaces(char *input) {
    while (isblank(*input)) input++;
    return input;
}

struct scan_result *single(char *input, char *keyword, struct location loc, enum token_type type) {
    struct scan_result *scan_result = NULL;
    struct token *token = NULL;
    loc->col++;
    token = init_token(*input, loc, NULL);
    scan_result = init_scan_result(token, input + 1);
    return scan_result;
}

struct scan_result *keyword(char *input, char *keyword, struct location loc, enum token_type type) {
    struct scan_result *scan_result = NULL;

    if (scan_result = string(input, keyword, loc, type)) {
        if (isalnum(*scan_result->input)) {
            free_scan_result(scan_result);
            scan_result = NULL;
        }
    }

    return scan_result;
}

struct scan_result *string(char *input, char *string, struct location loc, enum token_type type) {
    struct scan_result *scan_result = NULL;

    if (strcmp(input, string) == 0) {
        struct token *token = NULL;
        size_t slen = strlen(string);
        loc->col += slen;
        token = init_token(type, loc, NULL);
        scan_result = init_scan_result(token, input + slen);
    }

    return scan_result;
}

struct scan_result *number(char *input, char *string, struct location loc, enum token_type type) {
    char *prev = input;
    struct scan_result *scan_result = NULL;
    errno = 0;
    long num = strtol(input, &input, 0);
    if (!errno) {
        loc->col += input - prev;
        struct token *token = init_token(T_NUM, num, loc);
        scan_result = init_scan_result(token, input);
    }
    return scan_result;
}

struct scan_result *identifier(char *input, char *string, struct location loc, enum token_type type) {
    struct scan_result *scan_result = NULL;

    if (isalpha(*input)) {
        char *prev = input;
        while (isalnum(*input)) input++;
        size_t slen = input - prev;
        loc->col += slen;
        char *id = strndup(prev, slen);
        struct token *token = init_token(T_ID, id, loc);
        scan_result = init_scan_result(token, input);
    }

    return scan_result;
}

struct scan_result *token(char *input, struct location loc) {
    char *prev = input;
    struct scan_result *scan_result = NULL;

    input = spaces(input);
    loc->col += input - prev;

    if (input = character(input, '\n')) {
        loc->line++;
        loc->col = 0;

        if      (scan_result = keyword(input, "if", loc, T_IF)) {}
        else if (scan_result = keyword(input, "while", loc, T_WHILE)) {}
        else if (scan_result = keyword(input, "do", loc, T_DO)) {}
        else if (scan_result = string(input, "<=", loc, T_LT_EQ)) {}
        else if (scan_result = number(input, loc, T_NUM)) {}
        else if (scan_result = identifier(input, loc, T_ID)) {}
        else    {scan_result = single(input, loc); }
    }

    return scan_result;
}

struct list *tokens(char *input) {
}

struct token *init_token(short type, struct location loc, void *val) {
    struct token *token = malloc(sizeof *token);
    assert(token != NULL);

    switch (type) {
        case T_ID:
            token->id = val;
            break;
        case T_NUM:
            token->num = val;
            break;
        default:
            token->nothing = NULL;
            break;
    }

    token->loc = loc;
    token->type = type;

    return token;
}

short token_type(struct token *token) {
    return token->type;
}

struct location token_loc(struct token *token) {
    return token->loc;
}

void *token_val(struct token *token) {
    void *val = NULL;

    switch (token->type) {
        case T_ID:
            val = token->id;
            break;
        case T_NUM:
            val = token->num;
            break;
    };

    return val;
}

char *lexeme_for(short type) {
    char *lexeme;

    switch (type) {
        case T_EOF:   lexeme = "end of file";  break;
        case T_IF:    lexeme = "if";           break;
        case T_WHILE: lexeme = "while";        break;
        case T_DO:    lexeme = "do";           break;
        case T_LT_EQ: lexeme = "<=";           break;
        case T_ID:    lexeme = "[identifier]"; break;
        default:
            lexeme = calloc(2, sizeof *le);
            lexeme[0] = type;
            break;
    };

    return lexeme;
}

void free_token(struct token *token) {
    switch (token->type) {
        case T_ID:
            free(token->id);
            break;
    };

    free(token);
}
