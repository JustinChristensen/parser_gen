#ifndef SCANNER_H_
#define SCANNER_H_

#include <linked_list.h>
#include <stdlib.h>

enum token_type {
    T_IF = 256,
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
        long num;
    };
};

struct scan_result {
    struct token *token;
    char *input;
};

struct scan_result *init_scan_result(struct token *token, char *input);
struct location empty_loc();
void free_scan_result(struct scan_result *scan_result);
char *spaces(char *input);
char *character(char *input, char c);
struct scan_result *single(char *input, struct location loc);
struct scan_result *keyword(char *input, char *keyword, struct location loc, enum token_type type);
struct scan_result *string(char *input, char *string, struct location loc, enum token_type type);
struct scan_result *number(char *input, struct location loc);
struct scan_result *identifier(char *input, struct location loc);
struct scan_result *token(char *input, struct location loc);
struct list *tokens(char *input);
struct token *init_token(short type, struct location loc, void *val);
void free_token(struct token *token);
short token_type(struct token *token);
struct location token_loc(struct token *token);
void *token_val(struct token *token);
char *lexeme_for(short type);

#endif // SCANNER_H_
