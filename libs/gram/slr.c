#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/base.h>
#include <base/assert.h>
#include <base/debug.h>
#include <regex/nfa.h>
#include "gram/spec.h"
#include "gram/analyze.h"
#include "gram/states.h"
#include "gram/slr.h"

#include "internal/assert.c"
#include "internal/gen.c"
#include "internal/macros.c"

#define debug(...) debug_ns("gram_slr", __VA_ARGS__);

static bool _oom_error(struct slr_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct slr_error) { .type = GM_SLR_OOM_ERROR, .file = file, .col = col }));

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

static bool not_slr_error(struct slr_error *error) {
    prod(error, ((struct slr_error) { .type = GM_SLR_NOT_SLR_ERROR }));
    return false;
}

static bool scanner_error(struct slr_error *error, struct regex_error scanerr) {
    prod(error, ((struct slr_error) { .type = GM_SLR_SCANNER_ERROR, .scanerr = scanerr }));
    return false;
}

struct slr_parser slr_parser(
    struct nfa_context scanner, struct slr_action **atable,
    struct gram_stats stats
) {
    return (struct slr_parser) { atable, scanner, stats };
}

#define ACCEPT() (struct slr_action) { GM_SLR_ACCEPT }
#define SHIFT(num) (struct slr_action) { GM_SLR_SHIFT, (num) }
#define GOTO(num) (struct slr_action) { GM_SLR_GOTO, (num) }
static struct slr_action **action_table(unsigned nstates, struct lr_state *state, struct gram_stats const stats) {
    unsigned nsymbols = offs(stats.symbols);
    size_t atsize = nstates + nstates * nsymbols * sizeof (struct slr_action);

    struct slr_action **atable = malloc(atsize);
    if (!atable) return NULL;

    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return free(atable), NULL;

    memset(atable, 0, atsize);
    struct slr_action *rows = (struct slr_action *) (atable + nstates);

    gram_sym_no const nonterm0 = offs(stats.terms);

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (atable[state->num]) continue;

        struct slr_action *row = rows + state->num * nsymbols;
        atable[state->num] = row;

        struct lr_transitions *trans = state->trans;
        for (unsigned i = 0; i < trans->nstates; i++) {
            struct lr_state *next = trans->states[i];

            struct slr_action action;
            if (next->sym == GM_EOF)       action = ACCEPT();
            else if (next->sym < nonterm0) action = SHIFT(next->num);
            else                           action = GOTO(next->num);

            row[next->sym] = action;

            apush(&next, stack);
        }
    }

    free_array(stack);

    return atable;
}

bool gen_slr(
    struct slr_error *error, struct slr_parser *parser,
    struct gram_parser_spec *spec
) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);

    prod(error, ((struct slr_error) { 0 }));
    *parser = (struct slr_parser) { 0 };

    struct gram_stats stats = spec->stats;

    struct gram_symbol_analysis san = { 0 };
    struct gram_analysis gan = { 0 };
    struct nfa_context scanner = { 0 };
    unsigned nstates = 0;
    struct lr_state *states = NULL;
    struct slr_action **atable = NULL;

    if (!gram_analyze_symbols(&san, spec))
        return false;

    if (!gram_analyze(&gan, &san, spec))
        return oom_error(error, NULL);

    if (gan.clas < GM_SLR) {
        not_slr_error(error);
        goto free;
    }

    free_gram_analysis(&gan);

    if (!init_scanner(&scanner, spec->patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    if ((states = discover_lr_states(&nstates, spec)) == NULL)
        goto free;

    atable = action_table(nstates, states, spec->stats);
    if (!atable) {
        oom_error(error, NULL);
        goto free;
    }

    free_lr_states(nstates, states);
    free_gram_symbol_analysis(&san);

    *parser = slr_parser(scanner, atable, stats);

    return true;
free:
    free_gram_symbol_analysis(&san);
    free_nfa_context(&scanner);
    free_lr_states(nstates, states);
    free(atable);

    return false;
}

// void print_slr_parser(FILE *handle, struct slr_parser *parser);
// void fre_slr_parser(struct slr_parser *parser);
//
// void print_slr_error(FILE *handle, struct slr_error error);
