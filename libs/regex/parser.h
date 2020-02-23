#ifndef REGEX_PARSER_IH_
#define REGEX_PARSER_IH_ 1

#include <stdbool.h>
#include <regex/parser.h>

#define RESULTFN (union regex_result (*) (void *))
#define HASERRFN (bool (*) (void *))
#define ERRFN (struct regex_error (*) (void *))
#define ACTIONS (bool (*const *)(union regex_result, void *))

#define SYNERR_FMT_STRING "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNERR_FMT_STRING_END "\n|\n| At Column: %d\n|\n"
#define OOM_FMT_STRING "| OOM Error\n|\n"
#define REPEAT_ZERO_FMT_STRING "| Repeat Zero Error\n|\n"
#define MISSING_TAG_FMT_STRING "| Tag Exists Error\n|\n| Pattern %s already defined\n|\n"
#define TAG_EXISTS_FMT_STRING "| Missing Tag Error\n|\n| Pattern %s not defined\n|\n"

#define NUM_SYMBOLS (UNOPS_NT + 1)
#define NUM_TERMINALS (RBRACE_T + 1)
#define NUM_NONTERMINALS (NUM_SYMBOLS - NUM_TERMINALS)
#define NUM_ACTIONS ((DO_REPEAT_EXACT + 1) - NUM_SYMBOLS)
// non-terminal index
#define NTI(sym) (sym - NUM_TERMINALS)
// do action index
#define AI(sym) (sym - NUM_SYMBOLS)

union regex_result get_result(struct regex_parse_context *context);
bool result_has_error(struct regex_parse_context *context);
struct regex_error result_error(struct regex_parse_context *context);
bool do_action(enum regex_symbol action, union regex_result val, struct regex_parse_context *context);
void start_scanning(char *input, struct regex_parse_context *context);
bool peek(enum regex_symbol expected, struct regex_parse_context *context);
bool expect(enum regex_symbol expected, struct regex_parse_context *context);
enum regex_symbol lookahead(struct regex_parse_context *context);
union regex_result lookahead_val(struct regex_parse_context *context);
union regex_result tag_val(char *tag, struct regex_parse_context *context);
struct regex_error syntax_error(enum regex_symbol actual, int lexeme_col, enum regex_symbol expected);
struct regex_error oom_error();
struct regex_error repeat_zero_error();
struct regex_error tag_exists_error(char *tag);
struct regex_error missing_tag_error(char *tag);
bool set_syntax_error(enum regex_symbol expected, struct regex_parse_context *context);
struct regex_error nullerror();
char const *str_for_sym(enum regex_symbol token);

#endif // REGEX_PARSER_IH_
