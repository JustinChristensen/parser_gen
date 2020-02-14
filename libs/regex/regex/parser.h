#ifndef REGEX_PARSER_H_
#define REGEX_PARSER_H_ 1

#include <stdbool.h>
#include "result_types.h"
#include "base.h"

#define GETVALFN (union regex_result (*) (void *))
#define SYNERR_FMT_STRING "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNERR_FMT_STRING_END "\n|\n| At Column: %d\n|\n"
#define OOM_FMT_STRING "out of memory\n"
#define REPEAT_ZERO_FMT_STRING "cannot repeat zero times\n"

enum regex_symbol {
    ERROR,

    // terminals
    EOF_T,
    SYMBOL_T,
    RANGE_T,
    NUM_T,
    ID_T,
    ALT_T,
    STAR_T,
    PLUS_T,
    OPTIONAL_T,
    DOTALL_T,
    LPAREN_T,
    RPAREN_T,
    CLASS_T,
    NEG_CLASS_T,
    END_CLASS_T,
    ID_BRACE_T,
    LBRACE_T,
    RBRACE_T,

    // non-terminals
    REGEX_NT,
    EXPR_NT,
    ALT_NT,
    ALTS_NT,
    FACTOR_NT,
    FACTORS_NT,
    CHAR_CLASS_NT,
    RANGES_NT,
    UNOPS_NT,

    // actions
    DO_REGEX,
    DO_EMPTY,
    DO_ALT,
    DO_CAT,
    DO_SUB,
    DO_ID,
    DO_CHAR_CLASS,
    DO_NEG_CLASS,
    DO_DOTALL,
    DO_SYMBOL,
    DO_RANGES,
    DO_RANGE,
    DO_STAR,
    DO_PLUS,
    DO_OPTIONAL,
    DO_REPEAT_EXACT
};

#define NUM_SYMBOLS_ACTIONS (DO_OPTIONAL + 1)
#define NUM_SYMBOLS (UNOPS_NT + 1)
#define NUM_TERMINALS (RBRACE_T + 1)
#define NUM_NONTERMINALS (NUM_SYMBOLS - NUM_TERMINALS)
#define NUM_ACTIONS ((DO_REPEAT_EXACT + 1) - NUM_SYMBOLS)
// non-terminal index
#define NTI(sym) (sym - NUM_TERMINALS)
// do action index
#define AI(sym) (sym - NUM_SYMBOLS)

enum gram_production {
    ERROR_P,

    // $empty
    EMPTY_P,

    // regex = expr eof { regex };
    REGEX_EXPR_P,

    // expr = { empty } $empty;
    EXPR_EMPTY_P,
    // expr = alt alts;
    EXPR_ALT_P,

    // alt = { empty } factors;
    ALT_FACTOR_P,
    // alt = { empty } $empty;
    ALT_EMPTY_P,

    // alts  = '|' alt { alt } alts:
    ALTS_ALT_P,

    // factors = factor { cat } factors;
    FACTORS_FACTOR_P,

    // char_class = ']' { char_class };
    CHAR_CLASS_RBRACKET_P,

    // char_class = 'a-z' { range_head } ranges ']' { char_class };
    CHAR_CLASS_RANGES_P,

    // ranges      = 'a-z' { range } ranges
    RANGES_RANGE_P,

    // factor = '(' expr ')' { sub } unops;
    FACTOR_SUBEXPR_P,
    // factor = '{' id '}' { id } unops;
    FACTOR_ID_P,
    // factor = '[' char_class unops;
    FACTOR_CLASS_P,
    // factor = '[^' char_class { neg_char_class } unops;
    FACTOR_NEG_CLASS_P,
    // factor = '.' { dotall } unops;
    FACTOR_DOTALL_P,
    // factor = a { sym } unops;;
    FACTOR_SYMBOL_P,

    // unops = '*' { star } unops;
    UNOPS_STAR_P,
    // unops = '+' { plus } unops;
    UNOPS_PLUS_P,
    // unops = '?' { optional } unops;
    UNOPS_OPTIONAL_P,
    // unops = '{' number '}' { repeat_exact } unops;
    UNOPS_REPEAT_EXACT_P
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

enum error_type {
    SYNTAX_ERROR,
    OUT_OF_MEMORY,
    REGEX_NOT_DEFINED,
    REPEAT_ZERO
};

struct regex_error {
    enum error_type type;
    union {
        struct {
            int lexeme_col;
            enum regex_symbol actual;
            enum regex_symbol const *expected;
        };
        char *id;
    };
};

struct parse_context {
    void *result_context;
    union regex_result (*get_result)(void *result_context);
    bool (**actions)(union regex_result val, struct parse_context *pcontext);
    struct regex_token token;
    enum regex_symbol lookahead;
    int lookahead_col;
    union regex_token_val lookahead_val;
    bool use_nonrec;
    bool has_error;
    struct regex_error error;
};

struct regex_token regex_token(char *input);
struct regex_token scan(struct regex_token token);
enum regex_symbol token_type(struct regex_token token);
union regex_token_val token_val(struct regex_token token);
void token_lexeme(char *lbuf, struct regex_token token);
int token_col(struct regex_token token);
void print_token(struct regex_token token);
void print_token_table(char *regex);

union regex_result result(struct parse_context *context);
bool do_action(enum regex_symbol action, union regex_result val, struct parse_context *context);
struct parse_context parse_context(
    void *result_context,
    union regex_result (*get_result)(void *result_context),
    bool (**actions)(union regex_result val, struct parse_context *context),
    bool use_nonrec
);
void start_scanning(char *input, struct parse_context *context);
bool peek(enum regex_symbol expected, struct parse_context *context);
bool expect(enum regex_symbol expected, struct parse_context *context);
int is_symbol(int c);
enum regex_symbol lookahead(struct parse_context *context);
union regex_result lookahead_val(struct parse_context *context);
union regex_result id_val(char *id, struct parse_context *context);
struct regex_error syntax_error(
    enum regex_symbol actual,
    int lexeme_col,
    enum regex_symbol expected
);
struct regex_error oom_error();
struct regex_error repeat_zero_error();
bool set_syntax_error(enum regex_symbol expected, struct parse_context *context);
bool set_oom_error(struct parse_context *context);
bool set_repeat_zero_error(struct parse_context *context);
void print_regex_error(struct regex_error error);
bool has_parse_error(struct parse_context *context);
struct regex_error parse_error(struct parse_context *context);
struct regex_error nullperr();
char const *str_for_prod(enum gram_production p);
char const *str_for_sym(enum regex_symbol token);

#endif // REGEX_PARSER_H_
