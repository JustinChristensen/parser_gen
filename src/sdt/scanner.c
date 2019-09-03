#include <assert.h>
#include <stdlib.h>
#include "scanner.h"

// struct scan_result *token(char *input);
// struct token_list *tokens(char *input);

struct token *init_token(short type, void *val) {
    struct token *token = malloc(sizeof *token);
    assert(token != NULL);

    switch (type) {
        case T_ID:
            token->id = val;
            break;
        case T_NUM:
            token->num = *val;
            break;
        default:
            token->nothing = NULL;
            break;
    }

    token->type = type;

    return token;
}

short token_type(struct token *token) {
    return token->type;
}

void *token_val(struct token *token) {
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
