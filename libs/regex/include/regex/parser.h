#ifndef REGEX_PARSER_H_
#define REGEX_PARSER_H_ 1

#include <stdbool.h>
#include <regex/base.h>
#include <regex/result_types.h>

enum regex_symbol {
    RX_ERROR,

    // terminals
    RX_EOF_T,
    RX_CHAR_T,
    RX_RANGE_T,
    RX_NUM_T,
    RX_TAG_T,
    RX_ALT_T,
    RX_STAR_T,
    RX_PLUS_T,
    RX_OPTIONAL_T,
    RX_DOTALL_T,
    RX_LPAREN_T,
    RX_RPAREN_T,
    RX_CLASS_T,
    RX_NEG_CLASS_T,
    RX_END_CLASS_T,
    RX_TAG_BRACE_T,
    RX_LBRACE_T,
    RX_RBRACE_T,

    // non-terminals
    RX_REGEX_NT,
    RX_EXPR_NT,
    RX_ALT_NT,
    RX_ALTS_NT,
    RX_FACTOR_NT,
    RX_FACTORS_NT,
    RX_CHAR_CLASS_NT,
    RX_RANGES_NT,
    RX_UNOPS_NT,

    // actions
    RX_DO_REGEX,
    RX_DO_EMPTY,
    RX_DO_ALT,
    RX_DO_CAT,
    RX_DO_SUB,
    RX_DO_TAG,
    RX_DO_CHAR_CLASS,
    RX_DO_NEG_CLASS,
    RX_DO_DOTALL,
    RX_DO_CHAR,
    RX_DO_RANGES,
    RX_DO_RANGE,
    RX_DO_STAR,
    RX_DO_PLUS,
    RX_DO_OPTIONAL,
    RX_DO_REPEAT_EXACT
};

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

enum regex_error_type {
    RX_SYNTAX_ERROR,
    RX_OUT_OF_MEMORY,
    RX_REPEAT_ZERO,
    RX_TAG_EXISTS,
    RX_MISSING_TAG
};

struct regex_error {
    enum regex_error_type type;
    union {
        struct {
            int lexeme_col;
            enum regex_symbol actual;
            enum regex_symbol const *expected;
        };
        char *tag;
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
struct regex_parse_context parse_context(void *result, struct regex_parse_interface pi);
bool parse_regex_rec(char *pattern, struct regex_parse_context *context);
bool parse_regex_nonrec(char *pattern, struct regex_parse_context *context);
bool parse_regex(char *pattern, struct regex_parse_context *context);
bool regex_has_parse_error(struct regex_parse_context *context);
struct regex_error regex_parse_error(struct regex_parse_context *context);
void print_regex_error(struct regex_error error);

#endif // REGEX_PARSER_H_
