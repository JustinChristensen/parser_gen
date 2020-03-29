#ifndef GRAM_AST_IH_
#define GRAM_AST_IH_ 1

struct gram_pattern_def {
    char *id;
    char *regex;
    bool tag_only;
    bool skip;
    struct gram_pattern_def *next;
    size_t n;
};

struct gram_rule {
    char *id;
    struct gram_alt *alts;
    struct gram_rule *next;
    size_t n;
};

struct gram_alt {
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
    enum gram_rhs_type type;
    union {
        struct { char *str; };
    };
    struct gram_rhs *next;
    size_t n;
};

struct gram_pattern_def *init_gram_pattern_def(
    char *id, char *regex,
    bool tag_only, bool skip,
    struct gram_pattern_def *next
);
struct gram_rule *init_gram_rule(char *id, struct gram_alt *alts, struct gram_rule *next);
struct gram_alt *init_gram_alt(struct gram_rhs *rhses, struct gram_alt *next);
struct gram_rhs *init_id_gram_rhs(char *str, struct gram_rhs *next);
struct gram_rhs *init_char_gram_rhs(char *str, struct gram_rhs *next);
struct gram_rhs *init_string_gram_rhs(char *str, struct gram_rhs *next);
struct gram_rhs *init_empty_gram_rhs(struct gram_rhs *next);

void free_gram_parser_spec(struct gram_parser_spec *spec);
void free_gram_pattern_def(struct gram_pattern_def *pdef);
void free_gram_rule(struct gram_rule *rule);
void free_gram_alt(struct gram_alt *alt);
void free_gram_rhs(struct gram_rhs *rhs);

#endif // GRAM_AST_IH_


