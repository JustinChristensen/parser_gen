#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <linked_list.h>
#include <ctype.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include "scanner.h"

struct scan_context scan_context(struct token *token, struct location loc, char *input) {
    struct scan_context scan_context = { token, loc, input };
    return scan_context;
}

bool scanned(struct scan_context context) {
    return context.token != NULL;
}

struct location empty_loc() {
    struct location loc = { 1, 1 };
    return loc;
}

struct scan_context eof(struct scan_context context) {
    context.token = NULL;

    if (*context.input == '\0') {
        struct token *token = init_token(T_EOF, context.loc, NULL);
        context.loc.col++;
        context.input++;
        context.token = token;
    }

    return context;
}

struct scan_context single(struct scan_context context) {
    context.token = NULL;
    context.token = init_token(*context.input, context.loc, NULL);
    context.loc.col++;
    context.input++;
    return context;
}

struct scan_context keyword(struct scan_context context, char *keyword, enum token_type type) {
    context.token = NULL;
    struct scan_context next = string(context, keyword, type);

    if (scanned(next)) {
        if (isalnum(*next.input)) {
            free_token(next.token);
            next = context;
        }
    }

    return next;
}

struct scan_context string(struct scan_context context, char *string, enum token_type type) {
    context.token = NULL;
    size_t slen = strlen(string);

    if (strncmp(string, context.input, slen) == 0) {
        context.token = init_token(type, context.loc, NULL);
        context.loc.col += slen;
        context.input += slen;
    }

    return context;
}

struct scan_context number(struct scan_context context) {
    context.token = NULL;

    if (isdigit(*context.input)) {
        char *prev = context.input;
        while (isdigit(*context.input)) context.input++;
        size_t slen = context.input - prev;
        char *num = strndup(prev, slen);
        context.token = init_token(T_NUM, context.loc, num);
        context.loc.col += slen;
    }

    return context;
}

struct scan_context identifier(struct scan_context context) {
    context.token = NULL;

    if (isalpha(*context.input)) {
        char *prev = context.input;
        while (isalnum(*context.input)) context.input++;
        size_t slen = context.input - prev;
        char *id = strndup(prev, slen);
        context.token = init_token(T_ID, context.loc, id);
        context.loc.col += slen;
    }

    return context;
}

struct scan_context scan(struct scan_context context) {
    context.token = NULL;

    while (isspace(*context.input)) {
        if (*context.input == '\n') {
            context.loc.line++;
            context.loc.col = 1;
        } else {
            context.loc.col++;
        }

        context.input++;
    }

    if      (scanned(context = eof(context)));
    else if (scanned(context = keyword(context, "if", T_IF)));
    else if (scanned(context = keyword(context, "while", T_WHILE)));
    else if (scanned(context = keyword(context, "do", T_DO)));
    else if (scanned(context = string(context, "<=", T_LT_EQ)));
    else if (scanned(context = number(context)));
    else if (scanned(context = identifier(context)));
    else (context = single(context));

    return context;
}

struct token *token(struct scan_context context) {
    return context.token;
}

struct list *tokens(char *input) {
    struct list *list = init_list();
    struct scan_context context = scan(scan_context(NULL, empty_loc(), input));

    if (scanned(context)) {
        do { append(list, token(context)); }
        while (token_type(token(context)) != T_EOF && scanned(context = scan(context)));
    }

    return list;
}

void display_token(struct token *token) {
    printf("type: %d", token->type);

    switch (token->type) {
        case T_ID:
            printf(", value: %s", token->id);
            break;
        case T_NUM:
            printf(", value: %s", token->num);
            break;
    }
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
    char *lexeme = calloc(50, sizeof *lexeme);
    assert(lexeme != NULL);

    switch (type) {
        case T_EOF:   strcpy(lexeme, "[end of file]"); break;
        case T_IF:    strcpy(lexeme, "if");            break;
        case T_WHILE: strcpy(lexeme, "while");         break;
        case T_DO:    strcpy(lexeme, "do");            break;
        case T_LT_EQ: strcpy(lexeme, "<=");            break;
        case T_NUM:   strcpy(lexeme, "[number]");      break;
        case T_ID:    strcpy(lexeme, "[identifier]");  break;
        default:
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
        case T_NUM:
            free(token->num);
            break;
    };

    free(token);
}
