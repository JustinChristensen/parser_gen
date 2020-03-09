#ifndef GRAM_LR_H_
#define GRAM_LR_H_ 1

#include <base/array.h>
#include <base/hash_table.h>
#include <base/intset.h>
#include <regex/nfa.h>
#include "parser.h"

enum lr0_error_type {
    LR0_SCANNER_ERROR,
    LR0_OOM_ERROR
};

struct lr0_error {
    enum lr0_error_type type;
    union {
        struct regex_error scanerr;
    };
};

struct lr0_symbol {
    unsigned int sym;
    unsigned int str;
    struct regex_loc loc;
};

struct lr0_rule {
    struct lr0_symbol lhs;
    struct array *rhs;
};

struct lr0_state {
};

struct lr0_context {
    unsigned int nsyms;
    struct array *strings;
    struct hash_table *symtab;
    struct nfa_context scanner;
    bool has_error;
    struct lr0_error error;
};

bool lr0_context(struct lr0_context *context);
void free_lr0_context(struct lr0_context *context);

extern struct gram_result_interface gram_lr_iface;

#endif // GRAM_LR_H_
