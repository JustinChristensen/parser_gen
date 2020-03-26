#ifndef GRAM_PACK_H_
#define GRAM_PACK_H_ 1

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <regex/nfa.h>
#include "gram/ast.h"

struct gram_packed_spec {
    struct regex_pattern *patterns;
    struct gram_symbol *symbols;
    int **rules;
};

struct gram_packed_spec *gram_pack(struct gram_parser_spec *spec, struct hash_table *symtab, struct gram_stats stats);
void free_gram_packed_spec(struct gram_packed_spec *spec);

#endif // GRAM_PACK_H_

