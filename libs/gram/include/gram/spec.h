#ifndef GRAM_SPEC_H_
#define GRAM_SPEC_H_ 1

#include <stdbool.h>
#include <regex/base.h>

#define GM_EMPTY_TOKEN "$empty"

#define GM_END_PATTERN { 0 }
#define GM_START_SYMBOL { 0 }
#define GM_END_SYMBOL { 0 }
#define GM_START_RULE NULL
#define GM_END_RULE NULL

#define GM_EOF 1
#define GM_START 1
#define GM_SYMBOL0 1

#define GM_NUM_TYPE unsigned
typedef GM_NUM_TYPE gram_sym_no;
typedef GM_NUM_TYPE gram_rule_no;

enum gram_symbol_type {
    GM_TERM,
    GM_NONTERM
};

struct gram_symbol {
    enum gram_symbol_type type;
    gram_sym_no num;
    union {
        struct {
            gram_rule_no *derives;
        };
    };
};

struct gram_stats {
    unsigned patterns;
    unsigned terms;
    unsigned nonterms;
    unsigned symbols;
    unsigned rules;
    unsigned rsymbols;
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
            gram_sym_no **rules;
        };
    };
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

struct gram_parser_spec gram_parsed_spec(
    struct gram_pattern_def *pdefs, struct gram_rule *prules,
    struct gram_stats stats
);
struct gram_parser_spec gram_packed_spec(
    struct regex_pattern *patterns, struct gram_symbol *symbols,
    gram_sym_no **rules, struct gram_stats stats
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
struct gram_rhs *init_empty_gram_rhs(struct regex_loc loc, struct gram_rhs *next);

bool gram_rhses_empty(struct gram_rhs *rhses);

void free_gram_parser_spec(struct gram_parser_spec *spec);
void free_gram_pattern_def(struct gram_pattern_def *pdef);
void free_gram_rule(struct gram_rule *rule);
void free_gram_alt(struct gram_alt *alt);
void free_gram_rhs(struct gram_rhs *rhs);

bool gram_has_rules(struct gram_parser_spec const *spec);
bool gram_symbol_null(struct gram_symbol const *sym);
struct gram_symbol *gram_term0(struct gram_parser_spec const *spec);
struct gram_symbol *gram_nonterm0(struct gram_parser_spec const *spec);
struct gram_symbol *gram_symbol0(struct gram_parser_spec const *spec);
gram_sym_no **gram_rule0(struct gram_parser_spec const *spec);
struct gram_symbol *gram_start_sym(struct gram_parser_spec const *spec);

void print_gram_parser_spec(FILE *handle, struct gram_parser_spec const *spec);

#endif // GRAM_SPEC_H_

