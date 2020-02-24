#ifndef GRAM_PARSER_IH_
#define GRAM_PARSER_IH_ 1

#include "gram/parser.h"

#define RESULTFN (union gram_result (*) (void *))
#define HASERRFN (bool (*) (void *))
#define ERRFN (struct gram_error (*) (void *))
#define ACTIONS (bool (*const *)(union gram_result, void *))

#define SYNTAX_ERROR_FMT_START "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
#define SYNTAX_ERROR_FMT_END "\n|\n"

#define NUM_TERMINALS (GM_WHITESPACE_T + 1)
#define NUM_SYMBOLS (GM_RHS_NT + 1)
#define NUM_NONTERMINALS (NUM_SYMBOLS - NUM_TERMINALS)
#define NUM_ACTIONS ((GM_DO_RHSES_HEAD + 1) - NUM_NONTERMINALS)
#define AI(sym) (sym - NUM_NONTERMINALS)

static const union gram_result NULL_RESULT = { NULL };

#endif // GRAM_PARSER_IH_
