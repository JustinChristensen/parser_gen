#ifndef REGEX_PARSER_H_
#define REGEX_PARSER_H_ 1

#include <stdbool.h>
#include "result_types.h"
#include "base.h"

#define GETVALFN (union regex_result (*) (void *))
#define SYNERR_FMT_STRING "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNERR_FMT_STRING_END "\n|\n| At Column: %d\n|\n"
#define OOM_FMT_STRING "out of memory\n"

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

    // expr: ε
    // alt_tail: ε
    // cat_tail: ε
    // factor_tail: ε
    EMPTY_P,

    // regex: expr eof { regex }
    REGEX_P,

    // expr: alt
    EXPR_ALT_P,

    // alt: cat alt_tail
    ALT_CAT_P,

    // alt_tail: + cat { alt } alt_tail
    ALT_TAIL_CAT_P,

    // cat: ε { empty } factor cat_tail
    CAT_FACTOR_P,

    // cat_tail: factor { cat } cat_tail
    CAT_TAIL_FACTOR_P,

    // factor: ( expr ) { sub } factor_tail
    FACTOR_SUBEXPR_P,
    // factor: . { dotall } factor_tail
    FACTOR_DOTALL_P,
    // factor: a { sym } factor_tail
    FACTOR_SYMBOL_P,

    // factor_tail: * { star } factor_tail
    FACTOR_TAIL_STAR_P,
    // factor_tail: + { plus } factor_tail
    FACTOR_TAIL_PLUS_P,
    // factor_tail: ? { optional } factor_tail
    FACTOR_TAIL_OPTIONAL_P
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
    REGEX_NOT_DEFINED
};

struct parse_error {
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
    struct parse_error error;
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
bool set_syntax_error(enum regex_symbol expected, struct parse_context *context);
bool set_oom_error(struct parse_context *context);
void print_parse_error(struct parse_error error);
bool has_parse_error(struct parse_context *context);
struct parse_error parse_error(struct parse_context *context);
struct parse_error nullperr();
char *str_for_sym(enum regex_symbol token);

#endif // REGEX_PARSER_H_
