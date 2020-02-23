#ifndef REGEX_PARSER_H_
#define REGEX_PARSER_H_ 1

#include <stdbool.h>
#include <regex/base.h>

struct regex_token {
    char *input;
    int input_col;

    bool in_class;
    bool in_braces;

    struct {
        enum regex_symbol type;
        char *lexeme;
        int lexeme_col;
        union regex_token_val val;
    };
};

struct regex_parse_interface {
    union regex_result (*result)(void *result);
    bool (*has_error)(void *result);
    struct regex_error (*error)(void *result);
    bool (*const *actions)(union regex_result val, void *result);
};

struct regex_parse_context {
    void *result;
    struct regex_parse_interface pi;
    struct regex_token token;
    enum regex_symbol lookahead;
    int lookahead_col;
    union regex_token_val lookahead_val;
    bool has_error;
    struct regex_error error;
};

// scanner
struct regex_token regex_token(char *pattern);
struct regex_token regex_scan(struct regex_token token);
enum regex_symbol regex_token_type(struct regex_token token);
union regex_token_val regex_token_val(struct regex_token token);
void regex_token_lexeme(char *lexeme, struct regex_token token);
int regex_token_col(struct regex_token token);
void print_regex_token(struct regex_token token);
void print_regex_token_table(char *pattern);

// parsing
struct regex_parse_context regex_parse_context(void *result, struct regex_parse_interface pi);
bool parse_regex(char *pattern, struct regex_parse_context *context);
bool regex_has_parse_error(struct regex_parse_context *context);
struct regex_error regex_parse_error(struct regex_parse_context *context);

#endif // REGEX_PARSER_H_
