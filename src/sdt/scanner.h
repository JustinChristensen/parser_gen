#ifndef SCANNER_H_
#define SCANNER_H_

#include <stdlib.h>

enum token_type {
    T_EOF = 256
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
    union {
        void *nothing;
        char *id;
        int num;
    };
};

struct token_list {
    struct token *head;
    size_t size;
};

struct scan_result {
    struct token *token;
    char *input;
};

struct scan_result *token(char *input);
struct token_list *tokens(char *input);
struct token *init_token(short type, void *val);
short token_type(struct token *token);
void *token_val(struct token *token);
char *lexeme_for(short type);
void free_token(struct token *token);

#endif // SCANNER_H_
