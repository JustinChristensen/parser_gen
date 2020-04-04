#ifndef GRAM_PARSER_H_
#define GRAM_PARSER_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <base/array.h>
#include <base/hash_table.h>
#include <regex/nfa.h>
#include "gram/spec.h"

enum gram_symbol_entry_type {
    GM_SYMBOL_ENTRY,
    GM_PATTERN_ENTRY
};

struct gram_symbol_entry {
    enum gram_symbol_entry_type type;
    struct gram_symbol s;
    struct regex_loc first_loc;
    bool defined;
    int nrules;
};

enum gram_parser_symbol {
    GM_ERROR,

    // terminals
    GM_EOF_T,
    GM_TAG_ONLY_T,
    GM_SKIP_T,
    GM_REGEX_T,
    GM_SECTION_T,
    GM_ASSIGN_T,
    GM_ALT_T,
    GM_SEMICOLON_T,
    GM_CHAR_T,
    GM_STRING_T,
    GM_EMPTY_T,
    GM_END_T,
    GM_ID_T,

    // non-terminals
    GM_PARSER_SPEC_NT,
    GM_PATTERN_DEFS_NT,
    GM_PATTERN_DEF_NT,
    GM_GRAMMAR_NT,
    GM_RULES_NT,
    GM_RULE_NT,
    GM_ALTS_NT,
    GM_ALT_NT,
    GM_RHSES_NT,
    GM_RHS_NT
};

enum gram_parse_error_type {
    GM_PARSER_SYNTAX_ERROR,
    GM_PARSER_OOM_ERROR,
    GM_PARSER_PATTERN_DEFINED_ERROR,
    GM_PARSER_DUPLICATE_PATTERN_ERROR,
    GM_PARSER_NONTERM_DEFINED_AS_TERM_ERROR,
    GM_PARSER_SYMBOL_NOT_DEFINED_ERROR,
        GM_PARSER_SYMBOL_NOT_DERIVABLE_ERROR,
        GM_PARSER_MISSING_ACCEPTING_RULE,
        GM_PARSER_MULTIPLE_ACCEPTING_RULES,
    GM_PARSER_SCANNER_ERROR
};

struct gram_parse_error {
    enum gram_parse_error_type type;
    union {
        // syntax error, pattern defined, nonterm defined as term,
        // symbol not defined
        struct {
            struct regex_loc loc;
            union {
                struct { char *id; struct regex_loc prev_loc; };
                struct { enum gram_parser_symbol actual; enum gram_parser_symbol *expected; };
            };
        };
        // oom
        struct { char *file; int col; };
        // scanner error
        struct regex_error scanerr;
    };
};

struct gram_parse_context {
    struct hash_table *symtab;
    struct gram_stats stats;
    struct nfa_context scanner;
    struct nfa_match match;
    enum gram_parser_symbol sym;
    enum gram_parser_symbol *next_set;
};

bool gram_parse_context(struct gram_parse_error *error, struct gram_parse_context *context);
bool gram_start_scanning(struct gram_parse_error *error, char *input, struct gram_parse_context *context);
enum gram_parser_symbol gram_scan(struct gram_parse_context *context);
enum gram_parser_symbol gram_lookahead(struct gram_parse_context *context);
struct regex_loc gram_location(struct gram_parse_context *context);
bool gram_lexeme(char *lexeme, struct gram_parse_context *context);
bool gram_parse(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    char *input, struct gram_parse_context *context
);
void free_gram_parse_context(struct gram_parse_context *context);
void print_gram_parse_error(FILE *handle, struct gram_parse_error error);
void print_gram_tokens(FILE *handle, char *spec);

#endif // GRAM_PARSER_H_
