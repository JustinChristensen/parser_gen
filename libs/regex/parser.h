#ifndef REGEX_PARSER_IH_
#define REGEX_PARSER_IH_ 1

#include <stdbool.h>
#include "regex/parser.h"

#define RESULTFN (union regex_result (*) (void *))
#define HASERRFN (bool (*) (void *))
#define ERRFN (struct regex_error (*) (void *))
#define ACTIONS (bool (*const *)(union regex_result, void *))

#define NUM_SYMBOLS (RX_UNOPS_NT + 1)
#define NUM_TERMINALS (RX_RBRACE_T + 1)
#define NUM_NONTERMINALS (NUM_SYMBOLS - NUM_TERMINALS)
#define NUM_ACTIONS ((RX_DO_REPEAT_EXACT + 1) - NUM_SYMBOLS)
// non-terminal index
#define NTI(sym) (sym - NUM_TERMINALS)
// do action index
#define AI(sym) (sym - NUM_SYMBOLS)

#endif // REGEX_PARSER_IH_
