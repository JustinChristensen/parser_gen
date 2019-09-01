#include <assert.h>
#include <stdlib.h>
#include "scanner.h"

// struct scan_result *token(char *input);
// struct token_list *tokens(char *input);

struct token *init_token(short type, void *val) {
    struct token *token = malloc(sizeof *token);
    assert(token != NULL);

    switch (type) {
        // TODO: handle other token types
        default:
            token->nothing = NULL;
            break;
    }

    token->type = type;

    return token;
}

void free_token(struct token *token) {
    switch (token->type) {
        case T_ID:
            free(token->id);
            break;
    };

    free(token);
}

char *lexeme_for(short type) {
    char *le;

    switch (type) {
        case T_IF:    le = "if";           break;
        case T_WHILE: le = "while";        break;
        case T_DO:    le = "do";           break;
        case T_LT_EQ: le = "<=";           break;
        case T_ID:    le = "[identifier]"; break;
        default:
            le = calloc(2, sizeof *le);
            le[0] = type;
            break;
    };

    return le;
}
