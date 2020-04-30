#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <base/array.h>
#include <base/base.h>
#include <base/bitset.h>
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

static bool syntax_error(struct slr_error *error, gram_sym_no expected, struct slr_parser_state *state) {
    prod(error, ((struct slr_error) {
        .type = GM_SLR_SYNTAX_ERROR,
        .loc = nfa_match_loc(&state->match),
        .actual = state->lookahead,
        .expected = expected
    }));

    return false;
}

struct slr_parser slr_parser(
    unsigned nstates, struct slr_action **atable, struct nfa_context scanner,
    struct gram_stats const stats
) {
    return (struct slr_parser) { nstates, atable, scanner, stats };
}

static bool derived_by_table(gram_sym_no **derived_by, struct gram_parser_spec const *spec) {
    assert(derived_by != NULL);

    if (!gram_exists(spec)) return true;

    *derived_by = calloc(offs(spec->stats.rules), sizeof **derived_by);
    if (!*derived_by) return false;

    struct gram_symbol *nt = gram_nonterm0(spec);
    while (!gram_symbol_null(nt)) {
        gram_rule_no *r = nt->derives;
        while (*r) (*derived_by)[*r] = nt->num, r++;
        nt++;
    }

    return true;
}

#define ACCEPT() (struct slr_action) { GM_SLR_ACCEPT }
#define SHIFT(num) (struct slr_action) { GM_SLR_SHIFT, (num) }
#define REDUCE(num, nt) (struct slr_action) { GM_SLR_REDUCE, (num), (nt) }
#define GOTO(num) (struct slr_action) { GM_SLR_GOTO, (num) }
static struct slr_action **action_table(
    unsigned nstates, struct lr_state *state, struct gram_symbol_analysis const *san,
    struct gram_parser_spec const *spec
) {
    struct gram_stats const stats = spec->stats;
    unsigned nsymbols = offs(stats.symbols);
    size_t atsize = nstates * sizeof (struct slr_action *) + nstates * nsymbols * sizeof (struct slr_action);

    struct slr_action **atable = malloc(atsize);
    if (!atable) return NULL;

    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return free(atable), NULL;

    gram_sym_no *derived_by = NULL;
    if (!derived_by_table(&derived_by, spec)) return free(atable), free_array(stack), NULL;

    memset(atable, 0, atsize);
    struct slr_action *rows = (struct slr_action *) (atable + nstates);

    gram_sym_no const nonterm0 = offs(stats.terms);

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (atable[state->num]) continue;

        struct slr_action *row = rows + state->num * nsymbols;
        atable[state->num] = row;

        // reductions
        struct lr_itemset *itemset = state->itemset;
        for (unsigned i = 0; i < itemset->nitems; i++) {
            struct lr_item item = itemset->items[i];
            gram_sym_no *rule = spec->rules[item.rule];

            if (item.rule != GM_START && !rule[item.pos]) {
                gram_sym_no nt = derived_by[item.rule];
                struct bsiter it = bsiter(san->follows[nt]);
                gram_sym_no s;
                while (bsnext(&s, &it)) row[s] = REDUCE(item.pos, nt);
            }
        }

        // shifts, gotos, accept
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
    free(derived_by);

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

    if ((atable = action_table(nstates, states, &san, spec)) == NULL) {
        oom_error(error, NULL);
        goto free;
    }

    free_lr_states(nstates, states);
    free_gram_symbol_analysis(&san);

    *parser = slr_parser(nstates, atable, scanner, stats);

    return true;
free:
    free_gram_symbol_analysis(&san);
    free_gram_analysis(&gan);
    free_nfa_context(&scanner);
    free_lr_states(nstates, states);
    free(atable);

    return false;
}

static char action_sym(enum slr_action_type action) {
    switch (action) {
        case GM_SLR_ERROR:  return 'E';
        case GM_SLR_SHIFT:  return 'S';
        case GM_SLR_REDUCE: return 'R';
        case GM_SLR_GOTO:   return 'G';
        case GM_SLR_ACCEPT: return 'A';
    }
}

void print_slr_parser(FILE *handle, struct slr_parser *parser) {
    assert(parser != NULL);

    unsigned const nsymbols = offs(parser->stats.symbols);
    struct slr_action **atable = parser->atable;

    fprintf(handle, "action table:\n\n");

    for (gram_state_no st = 0; st < parser->nstates; st++) {
        for (gram_sym_no s = GM_SYMBOL0; s < nsymbols; s++) {
            struct slr_action act = atable[st][s];

            if (act.action) {
                char *fmt = "  atable[%u][%u] = (%c, %u)\n";

                if (act.action == GM_SLR_REDUCE)
                    fmt = "  atable[%u][%u] = (%c, %u, %u)\n";
                else if (act.action == GM_SLR_ACCEPT)
                    fmt = "  atable[%u][%u] = %c\n";

                fprintf(handle, fmt, st, s, action_sym(act.action), act.n, act.nt);
            }
        }
    }

    fprintf(handle, "\n");
}

void free_slr_parser(struct slr_parser *parser) {
    if (!parser) return;
    free(parser->atable);
    free_nfa_context(&parser->scanner);
    *parser = (struct slr_parser) { 0 };
}

struct slr_parser_state slr_parser_state(struct slr_parser *parser) {
    assert(parser != NULL);
    return (struct slr_parser_state) { .parser = parser };
}

static bool start_scanning(char *input, struct slr_parser_state *state) {
    if (nfa_start_match(input, &state->match, &state->parser->scanner)) {
        state->lookahead = nfa_match(&state->match);
        debug("initial lookahead: %u\n", state->lookahead);
        return true;
    }

    return false;
}

static void scan(struct slr_parser_state *state) {
    state->lookahead = nfa_match(&state->match);
}

static void debug_parser_state(struct array *states, struct slr_parser_state *state) {
    gram_state_no st;
    debug("(");
    for (unsigned i = 0, ssize = asize(states); i < ssize; i++) {
        at(&st, i, states);
        debug("%u ", st);
    }
    debug(", %u, ", state->lookahead);
    debug("%-.30s", state->match.input);
    debug(")\n");
}

#define STATES_STACK_SIZE 7
bool slr_parse(struct slr_error *error, char *input, struct slr_parser_state *state) {
    assert(state != NULL);

    if (!start_scanning(input, state)) return oom_error(error, NULL);

    struct array *states = init_array(sizeof (gram_state_no), STATES_STACK_SIZE, 0, 0);
    if (!states) return oom_error(error, NULL);

    struct slr_parser const *parser = state->parser;
    struct slr_action **atable = parser->atable;

    bool success = true;
    gram_state_no s = 0;
    apush(&s, states);
    while (success) {
        apeek(&s, states);

        debug_parser_state(states, state);

        if (state->lookahead == RX_REJECTED) {
            success = syntax_error(error, 0, state);
            break;
        }

        struct slr_action act = atable[s][state->lookahead];

        debug("action: %c\n+++\n", action_sym(act.action));

        if (act.action == GM_SLR_SHIFT) {
            apush(&act.n, states);
            scan(state);
        } else if (act.action == GM_SLR_REDUCE) {
            while (act.n) apop(&s, states), act.n--;
            apeek(&s, states);
            act = atable[s][act.nt];
            apush(&act.n, states);
        } else if (act.action == GM_SLR_ACCEPT) {
            break;
        } else success = syntax_error(error, 0, state);
    }

    free_array(states);

    return success;
}

void free_slr_parser_state(struct slr_parser_state *state) {
    free_nfa_match(&state->match);
}

#define ERROR_FMT_END "\n|\n"
#define OOM_ERROR_FMT_START "| SLR Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
#define NOT_SLR_FMT "| Grammar Not SLR\n|\n"
#define SYNTAX_ERROR_FMT_START "| SLR Syntax Error\n|\n| Got: %u\n| Expected: %u"
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
void print_slr_error(FILE *handle, struct slr_error error) {
    switch (error.type) {
        case GM_SLR_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, error.actual, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_SLR_NOT_SLR_ERROR:
            fprintf(handle, NOT_SLR_FMT);
            break;
        case GM_SLR_SCANNER_ERROR:
            print_regex_error(handle, error.scanerr);
            break;
        case GM_SLR_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
    }
}

