#ifndef GRAM_ASSERT_C_
#define GRAM_ASSERT_C_ 1

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <base/assert.h>
#include <base/bitset.h>
#include "gram/parser.h"
#include "gram/spec.h"
#include "gram/lr.h"
#include "gram/states.h"

#ifdef INVARIANTS

INVARIANT(assert_parsed_spec, struct gram_parser_spec const *spec) {
    check(spec != NULL);
    check(spec->type == GM_PARSED_SPEC);
}

INVARIANT(assert_packed_spec, struct gram_parser_spec const *spec) {
    check(spec != NULL);
    check(spec->type == GM_PACKED_SPEC);

    struct gram_stats const stats = spec->stats;

    // try to enforce null-termination
    if (stats.patterns) {
        check(spec->patterns != NULL);
        check(regex_null_pattern(&spec->patterns[stats.patterns]));
    }

    check(stats.symbols > 0);
    check(spec->symbols != NULL);
    check(spec->symbols[0].num == 0);
    check(spec->symbols[0].derives == NULL);
    check(gram_symbol_null(&spec->symbols[stats.symbols + 1]));

    check(stats.rules > 0);
    check(spec->rules != NULL);
    check(spec->rules[0] == NULL);
    check(spec->rules[stats.rules + 1] == NULL);

    if (!gram_exists(spec)) check(spec->rules[1][0] == GM_EOF);
    else {
        check(spec->rules[1][1] == GM_EOF);

        struct bitset *ruleset = bitset(stats.rules + 1);
        if (ruleset) {
            struct gram_symbol *nt = gram_nonterm0(spec);
            while (!gram_symbol_null(nt)) {
                gram_rule_no *r = nt->derives;

                while (*r) {
                    if (bselem(*r, ruleset)) {
                        fprintf(stderr, "rule %u derived by %u is already being derived by a prior non-terminal\n", *r, nt->num);
                        abort();
                    }

                    bsins(*r, ruleset);
                    r++;
                }

                nt++;
            }

            free(ruleset);
        }
    }
}

INVARIANT(assert_symbol_index, gram_sym_no i, struct gram_parser_spec const *spec) {
    check(i >= 1 && i <= spec->stats.symbols);
}

INVARIANT(assert_rule_index, gram_rule_no i, struct gram_parser_spec const *spec) {
    check(i >= 1 && i <= spec->stats.rules);
}

INVARIANT(assert_itemsets_sorted, struct lr_itemset const *kernel, struct lr_itemset const *itemset) {
    check(kernel != NULL);
    check(itemset != NULL);
    check(kernel->kitems > 0);
    check(kernel->nitems > 0);
    check(itemset->kitems > 0);
    check(itemset->nitems > 0);

    if (!lr_itemset_sorted(kernel)) {
        print_lr_itemset_compact(stderr, kernel);
        fprintf(stderr, "\n");
        fprintf(stderr, "kernel not sorted\n");
        abort();
    }

    if (!lr_itemset_sorted(itemset)) {
        print_lr_itemset_compact(stderr, itemset);
        fprintf(stderr, "\n");
        fprintf(stderr, "itemset not sorted\n");
        abort();
    }
}

#endif

#endif // GRAM_ASSERT_C_
