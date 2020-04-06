#ifndef GRAM_AST_H_
#define GRAM_AST_H_ 1

#include <stdbool.h>
#include <regex/base.h>

#define GM_EMPTY_TOKEN  "$empty"
#define GM_EOF_TOKEN    "$eof"
#define GM_EOF_NUM      0

#define GM_END_PATTERN  { 0 }
#define GM_START_SYMBOL { 0 }
#define GM_END_SYMBOL   { 0 }
#define GM_START_RULE   NULL
#define GM_END_RULE     NULL

enum gram_symbol_type {
    GM_TERM,
    GM_NONTERM
};

struct gram_symbol {
    enum gram_symbol_type type;
    int num;
    bool nullable;
    union {
        struct {
            unsigned int *rules;  // nonterm derives
        };
    };
};

struct gram_stats {
    unsigned int patterns;
    unsigned int terms;
    unsigned int nonterms;
    unsigned int symbols;
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
            unsigned int **rules;
        };
    };
    int start_rule;
    struct gram_stats stats;
};

struct gram_pattern_def {
    struct regex_loc loc;
    char *id;
    char *regex;
    bool tag_only;
    bool skip;
    struct gram_pattern_def *next;
    size_t n;
};

struct gram_rule {
    struct regex_loc loc;
    char *id;
    struct gram_alt *alts;
    struct gram_rule *next;
    size_t n;
};

struct gram_alt {
    struct regex_loc loc;
    struct gram_rhs *rhses;
    struct gram_alt *next;
    size_t n;
};

enum gram_rhs_type {
    GM_ID_RHS,
    GM_CHAR_RHS,
    GM_STRING_RHS,
    GM_EOF_RHS,
    GM_EMPTY_RHS
};

struct gram_rhs {
    struct regex_loc loc;
    enum gram_rhs_type type;
    union {
        struct { char *str; };
    };
    struct gram_rhs *next;
    size_t n;
};

#ifdef INVARIANTS
void assert_gram_parsed_spec(struct gram_parser_spec const *spec);
void assert_gram_packed_spec(struct gram_parser_spec const *spec);
void assert_gram_sym_index(unsigned int i, struct gram_stats stats);
#endif

struct gram_parser_spec gram_parsed_spec(
    struct gram_pattern_def *pdefs, struct gram_rule *rules,
    int start_rule, struct gram_stats stats
);
struct gram_parser_spec gram_packed_spec(
    struct regex_pattern *patterns, struct gram_symbol *symbols, unsigned int **rules,
    unsigned int start_rule, struct gram_stats stats
);
struct gram_pattern_def *init_gram_pattern_def(
    struct regex_loc loc,
    char *id, char *regex,
    bool tag_only, bool skip,
    struct gram_pattern_def *next
);
struct gram_rule *init_gram_rule(struct regex_loc loc, char *id, struct gram_alt *alts, struct gram_rule *next);
struct gram_alt *init_gram_alt(struct regex_loc loc, struct gram_rhs *rhses, struct gram_alt *next);
struct gram_rhs *init_id_gram_rhs(struct regex_loc loc, char *str, struct gram_rhs *next);
struct gram_rhs *init_char_gram_rhs(struct regex_loc loc, char *str, struct gram_rhs *next);
struct gram_rhs *init_string_gram_rhs(struct regex_loc loc, char *str, struct gram_rhs *next);
struct gram_rhs *init_eof_gram_rhs(struct regex_loc loc, struct gram_rhs *next);
struct gram_rhs *init_empty_gram_rhs(struct regex_loc loc, struct gram_rhs *next);

void free_gram_parser_spec(struct gram_parser_spec *spec);
void free_gram_pattern_def(struct gram_pattern_def *pdef);
void free_gram_rule(struct gram_rule *rule);
void free_gram_alt(struct gram_alt *alt);
void free_gram_rhs(struct gram_rhs *rhs);

void print_gram_parser_spec(FILE *handle, struct gram_parser_spec const *spec);
void print_gram_stats(FILE *handle, struct gram_parser_spec const *spec);

#endif // GRAM_AST_H_

