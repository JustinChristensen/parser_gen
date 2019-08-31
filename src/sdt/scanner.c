#include <assert.h>
#include <stdlib.h>
#include "scanner.h"

struct token *init_token(enum token_type type, void *val) {
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
    free(token);
}
