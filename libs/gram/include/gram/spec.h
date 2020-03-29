#ifndef GRAM_SPEC_H_
#define GRAM_SPEC_H_ 1

#include <stdlib.h>
#include <stdbool.h>

#include "ast.h"

enum gram_symbol_type {
    GM_TERM,
    GM_NONTERM
};

struct gram_symbol {
    enum gram_symbol_type type;
    int num;
    union {
        struct {
            int *rules;  // nonterm derives
            int nrules;
        };
    };
};

struct gram_stats {
    unsigned int patterns;
    unsigned int terms;
    unsigned int nonterms;
    unsigned int rules;
};

enum gram_parser_spec_type {
    GM_PARSED_SPEC,
    GM_CHECKED_SPEC,
    GM_PACKED_SPEC
};

struct gram_parser_spec {
    enum gram_parser_spec_type type;
    union {
        struct { // parsed, checked
            struct gram_pattern_def *pdefs;
            struct gram_rule *prules;
        };
        struct { // packed
            struct regex_pattern *patterns;
            struct gram_symbol *symbols;
            int **rules;
        };
    };
    struct gram_stats stats;
};

enum gram_spec_error_type {
    GM_SPEC_OOM_ERROR
};

struct gram_spec_error {
    enum gram_spec_error_type type;
    union {
        struct { char *file; int col; };
    };
};

#define GM_END_PATTERN  { 0 }
#define GM_START_SYMBOL { 0 }
#define GM_END_SYMBOL   { 0 }
#define GM_START_RULE   NULL
#define GM_END_RULE     NULL

#ifdef INVARIANTS
void assert_gram_parsed_spec(struct gram_packed_spec const *spec);
void assert_gram_packed_spec(struct gram_packed_spec const *spec);
#endif

struct gram_parser_spec gram_parsed_spec(struct gram_pattern_def *pdefs, struct gram_rule *rules);
struct gram_parser_spec gram_packed_spec(struct regex_pattern *patterns, struct gram_symbol *symbols, int **rules);
bool gram_check(struct gram_spec_error *error, struct gram_parser_spec *spec, struct hash_table *symtab);
bool gram_pack(struct gram_spec_error *error, struct gram_parser_spec *spec, struct hash_table *symtab);
void free_gram_parser_spec(struct gram_parser_spec *spec);
void print_gram_parser_spec(FILE *handle, struct gram_packed_spec const *spec);

#endif // GRAM_SPEC_H_


