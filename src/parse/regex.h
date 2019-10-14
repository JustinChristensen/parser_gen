#ifndef PARSE_REGEX_H_
#define PARSE_REGEX_H_ 1

#include <stdbool.h>

struct dfa_state {
    struct set *positions;
};

struct regex_error {
};

struct regex_context {
    bool has_error;
    struct regex_error error;
};

struct match {
    char *input;
    char *lexeme;
    int id;
};

struct regex_context regex_context();
bool parse_regex(struct regex *context);
void regex(char *regex, int match_id, struct regex_context *context);
int match(char *input, struct regex_context *context);

#endif // PARSE_REGEX_H_
