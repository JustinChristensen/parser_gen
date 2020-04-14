#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <base/assert.h>
#include "gram/ll1.h"
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"

static unsigned int **parse_table(
    bool const *nullable, struct intset **firsts, struct intset **follows,
    struct gram_parser_spec const *spec
) {
}

static unsigned int **rule_table(struct gram_parser_spec const *spec) {
}

bool genll1(struct ll1_error *error, struct ll1_parser *parser, struct gram_parser_spec *spec) {
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);

    prod(error, (struct ll1_error) { 0 });
    *parser = (struct ll1_parser) { 0 };

    bool *nullable = NULL;
    struct intset **firsts = NULL, **follows = NULL;
    struct nfa_context scanner = { 0 };
    unsigned int **parse = NULL, **rules = NULL;

    nullable = gram_nullable(spec);
    if (!nullable) return oom_error(error, NULL);

    firsts = gram_firsts(nullable, spec);
    if (!firsts) return oom_error(error, nullable);

    follows = gram_follows(nullable, firsts, spec);
    if (!follows) {
        oom_error(error, NULL);
        goto free;
    }

    if (!gram_is_ll1(error, nullable, firsts, follows, spec))
        goto free;

    if (!nfa_context(&scanner, spec->patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    unsigned int **ptable = parse_table(nullable, firsts, follows, spec);
    if (!ptable) {
        oom_error(error, NULL);
        goto free;
    }

    unsigned int **rtable = rule_table(spec);
    if (!rtable) {
        oom_error(error, NULL);
        goto free;
    }

    *parser = ll1_parser(scanner, ptable, rtable);

    return true;
free:
    free(nullable);
    free_gram_sets(firsts, spec);
    free_gram_sets(follows, spec);
    free_nfa_context(&scanner);
    free(ptable);
    free(rtable);

    return false;
}

