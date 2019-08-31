#ifndef SCANNER_H_
#define SCANNER_H_

#include <stdlib.h>

enum token_type {
    T_IF = 256,
    T_WHILE,
    T_DO,
    T_LTEQ,
    T_ID
};

struct token {
    short type; 
    union {     
        void *nothing;
        char *id;
    };
};

struct token_list {
    struct token *head;
    size_t size;
};

struct token *token(char *input);
struct token_list *tokens(char *input);
struct token *init_token(enum token_type type, void *val);
void free_token(struct token *token);

#endif // SCANNER_H_
