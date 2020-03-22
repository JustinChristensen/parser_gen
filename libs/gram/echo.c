#include <stdio.h>
#include <stdlib.h>
#include "gram/echo.h"

void echo_gram_pspec_stats(FILE *handle, struct gram_parser_spec *spec) {
    if (spec->pdefs)
        fprintf(handle, "num pattern defs: %ld\n", spec->pdefs->n);

    if (spec->rules) {
        struct gram_rule *rule = spec->rules;

        fprintf(handle, "num rules: %ld\n", rule->n);

        for (int rn = 1; rule; rule = rule->next) {
            fprintf(handle, "rule #%-d:\n", rn);

            struct gram_alt *alt = rule->alts;

            fprintf(handle, "\tnum alts: %ld\n", alt->n);

            for (int an = 1; alt; alt = alt->next) {
                fprintf(handle, "\talt #%-d:\n", an);
                fprintf(handle, "\t\tnum rhses: %ld\n", alt->rhses->n);
                an++;
            }

            rn++;
        }
    }
}

void echo_gram_pspec(FILE *handle, struct gram_parser_spec *spec) {
    echo_gram_pattern_def(handle, spec->pdefs);
    if (spec->rules) {
        fprintf(handle, "---\n");
        echo_gram_rule(handle, spec->rules);
    }
}

#define PATTERN_DEF_FMT "%s %s\n"
void echo_gram_pattern_def(FILE *handle, struct gram_pattern_def *def) {
    for (; def; def = def->next)
        fprintf(handle, PATTERN_DEF_FMT, def->id, def->regex);
}

#define RULE_FMT "%s = "
#define RULE_END_FMT ";\n"
void echo_gram_rule(FILE *handle, struct gram_rule *rule) {
    for (; rule; rule = rule->next) {
        fprintf(handle, RULE_FMT, rule->id);
        echo_gram_alt(handle, rule->alts);
        fprintf(handle, RULE_END_FMT);
    }
}

#define ALT_FMT " | "
void echo_gram_alt(FILE *handle, struct gram_alt *alt) {
    if (alt) {
        echo_gram_rhs(handle, alt->rhses);

        for (alt = alt->next; alt; alt = alt->next) {
            fprintf(handle, ALT_FMT);
            echo_gram_rhs(handle, alt->rhses);
        }
    }
}

#define ID_RHS_FMT "%s"
#define CHAR_RHS_FMT "'%s'"
#define STRING_RHS_FMT "\"%s\""
#define EMPTY_RHS_FMT "$empty"
#define RHS_SEP_FMT " "
static void _echo_gram_rhs(FILE *handle, struct gram_rhs *rhs) {
    switch (rhs->type) {
        case GM_ID_RHS:
            fprintf(handle, ID_RHS_FMT, rhs->str);
            break;
        case GM_CHAR_RHS:
            fprintf(handle, CHAR_RHS_FMT, rhs->str);
            break;
        case GM_STRING_RHS:
            fprintf(handle, STRING_RHS_FMT, rhs->str);
            break;
        case GM_EMPTY_RHS:
            fprintf(handle, EMPTY_RHS_FMT);
            break;
    }
}

void echo_gram_rhs(FILE *handle, struct gram_rhs *rhs) {
    if (rhs) {
        _echo_gram_rhs(handle, rhs);
        for (rhs = rhs->next; rhs; rhs = rhs->next) {
            fprintf(handle, RHS_SEP_FMT);
            _echo_gram_rhs(handle, rhs);
        }
    }
}
