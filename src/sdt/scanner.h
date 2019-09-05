#ifndef SCANNER_H_
#define SCANNER_H_

#include <linked_list.h>
#include <stdlib.h>
#include <stdbool.h>

enum token_type {
    T_EOF = 256,
    T_IF,
    T_WHILE,
    T_DO,
    T_LT_EQ,
    T_ID,
    T_NUM
};

struct location {
    int line;
    int col;
};

struct token {
    short type;
    struct location loc;
    size_t len;
    union {
        void *nothing;
        char *id;
        long *num;
    };
};

struct scan_context {
    struct token *token;
    struct location loc;
    char *input;
};

struct scan_context scan_context(struct token *token, struct location loc, char *input);
bool scanned(struct scan_context context);
struct location empty_loc();
struct scan_context eof(struct scan_context context);
struct scan_context single(struct scan_context context);
struct scan_context keyword(struct scan_context context, char *keyword, enum token_type type);
struct scan_context string(struct scan_context context, char *string, enum token_type type);
struct scan_context number(struct scan_context context);
struct scan_context identifier(struct scan_context context);
struct scan_context scan(struct scan_context context);
struct token *token(struct scan_context context);
struct list *tokens(char *input);
void display_token(struct token *token);
struct token *init_token(short type, struct location loc, void *val);
void free_token(struct token *token);
short token_type(struct token *token);
struct location token_loc(struct token *token);
void *token_val(struct token *token);
char *lexeme_for(short type);

#endif // SCANNER_H_
