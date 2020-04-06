#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/hash_table.h>
#include <regex/base.h>
#include "gram/check.h"
#include "gram/parser.h"
#include "gram/spec.h"

static char *permastr(char *str) {
    static char buf[BUFSIZ];
    strcpy(buf, str);
    return buf;
}

static bool symbol_not_defined_error(
    struct gram_parse_error *error,
    char *str, struct regex_loc loc
) {
    *error = (struct gram_parse_error) {
        GM_PARSER_SYMBOL_NOT_DEFINED_ERROR,
        .id = permastr(str),
        .loc = loc
    };

    return false;
}

static bool missing_start_rule_error(struct gram_parse_error *error) {
    *error = (struct gram_parse_error) { GM_PARSER_MISSING_START_RULE_ERROR };
    return false;
}

static bool entry_not_defined(struct gram_symbol_entry *entry) {
    assert(entry != NULL);
    return entry->type == GM_SYMBOL_ENTRY && !entry->defined;
}

bool gram_check(struct gram_parse_error *error,
    struct gram_parser_spec *spec,
    struct gram_parse_context *context
) {
    if (spec->type == GM_CHECKED_SPEC || spec->type == GM_PACKED_SPEC) return true;

    struct hash_table *symtab = context->symtab;

    // FIXME: check reachability of symbols from the start rule

    bool eof_seen = false;
    struct gram_rule *rule = NULL;
    for (rule = spec->prules; rule; rule = rule->next) {
        struct gram_alt *alt = NULL;
        for (alt = rule->alts; alt; alt = alt->next) {
            struct gram_rhs *rhs = NULL;
            for (rhs = alt->rhses; rhs; rhs = rhs->next) {
                switch (rhs->type) {
                    case GM_ID_RHS:
                    case GM_CHAR_RHS:
                    case GM_STRING_RHS:
                        if (entry_not_defined(htlookup(rhs->str, symtab)))
                            return symbol_not_defined_error(error, rhs->str, rhs->loc);
                        break;
                    case GM_EOF_RHS:
                        eof_seen = true;
                        break;
                    case GM_EMPTY_RHS:
                        break;
                }
            }
        }
    }

    if (spec->prules && !eof_seen)
        return missing_start_rule_error(error);

    spec->type = GM_CHECKED_SPEC;

    return true;
}

