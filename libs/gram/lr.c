#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <base/array.h>
#include <base/base.h>
#include <base/assert.h>
#include <base/debug.h>
#include <regex/nfa.h>
#include "gram/spec.h"
#include "gram/analyze.h"

#include "internal/assert.c"
#include "internal/gen.c"
#include "internal/lr.c"
#include "internal/macros.c"
#include "internal/spec.c"

#define debug(...) debug_ns("gram_lr", __VA_ARGS__);

static bool scanner_error(struct lr_error *error, struct regex_error scanerr) {
    prod(error, ((struct lr_error) { .type = GM_LR_SCANNER_ERROR, .scanerr = scanerr }));
    return false;
}

static bool syntax_error(struct lr_error *error, gram_sym_no expected, struct lr_parser_state *state) {
    prod(error, ((struct lr_error) {
        .type = GM_LR_SYNTAX_ERROR,
        .loc = nfa_match_loc(&state->match),
        .actual = state->lookahead,
        .expected = expected
    }));

    return false;
}

struct lr_parser lr_parser(
    unsigned nstates, struct lr_action **atable, struct nfa_context scanner,
    struct gram_stats const stats
) {
    return (struct lr_parser) { nstates, atable, scanner, stats };
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

bool gen_lr(
    struct lr_error *error, struct lr_parser *parser,
    struct lr_action **(*action_table)(
        struct lr_error *error, unsigned *nstates,
        struct gram_analysis const *gan, struct gram_symbol_analysis const *san, gram_sym_no const *derived_by,
        struct gram_parser_spec const *spec
    ),
    struct gram_parser_spec *spec
) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);
    assert(action_table != NULL);

    prod(error, ((struct lr_error) { 0 }));
    *parser = (struct lr_parser) { 0 };

    struct gram_stats stats = spec->stats;

    struct gram_symbol_analysis san = { 0 };
    struct gram_analysis gan = { 0 };
    gram_sym_no *derived_by = NULL;
    struct nfa_context scanner = { 0 };
    struct lr_action **atable = NULL;

    if (!gram_analyze_symbols(&san, spec))
        return oom_error(error, NULL);

    if (!gram_analyze(&gan, &san, spec)) {
        oom_error(error, NULL);
        goto free;
    }

    if (!derived_by_table(&derived_by, spec)) {
        oom_error(error, atable);
        goto free;
    }

    if (!init_scanner(&scanner, spec->patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    unsigned nstates = 0;
    if ((atable = (*action_table)(error, &nstates, &gan, &san, derived_by, spec)) == NULL)
        goto free;

    free_gram_analysis(&gan);
    free_gram_symbol_analysis(&san);
    free(derived_by);

    *parser = lr_parser(nstates, atable, scanner, stats);

    return true;
free:
    free_gram_symbol_analysis(&san);
    free_gram_analysis(&gan);
    free(derived_by);
    free_nfa_context(&scanner);
    free(atable);

    return false;
}

static char action_sym(enum lr_action_type action) {
    switch (action) {
        case GM_LR_ERROR:  return 'E';
        case GM_LR_SHIFT:  return 'S';
        case GM_LR_REDUCE: return 'R';
        case GM_LR_GOTO:   return 'G';
        case GM_LR_ACCEPT: return 'A';
    }
}

static void print_action(FILE *handle, struct lr_action act) {
    if (!act.action) return;

    char *fmt = "\"%c(%u)\"";

    if (act.action == GM_LR_REDUCE)
        fmt = "\"%c(%u, %u)\"";
    else if (act.action == GM_LR_ACCEPT)
        fmt = "\"%c\"";

    fprintf(handle, fmt, action_sym(act.action), act.n, act.nt);
}

void print_lr_parser(FILE *handle, struct lr_parser *parser) {
    assert(parser != NULL);

    struct lr_action **atable = parser->atable;

    FOR_SYMBOL(parser->stats, s) fprintf(handle, ",%u", s);
    fprintf(handle, "\n");

    for (gram_state_no st = 0; st < parser->nstates; st++) {
        fprintf(handle, "%u", st);

        FOR_SYMBOL(parser->stats, s) {
            fprintf(handle, ",");
            print_action(handle, atable[st][s]);
        }

        fprintf(handle, "\n");
    }
}

void free_lr_parser(struct lr_parser *parser) {
    if (!parser) return;
    free(parser->atable);
    free_nfa_context(&parser->scanner);
    *parser = (struct lr_parser) { 0 };
}

struct lr_parser_state lr_parser_state(struct lr_parser *parser) {
    assert(parser != NULL);
    return (struct lr_parser_state) { .parser = parser };
}

static bool start_scanning(char *input, struct lr_parser_state *state) {
    if (nfa_start_match(input, &state->match, &state->parser->scanner)) {
        state->lookahead = nfa_match(&state->match);
        debug("initial lookahead: %u\n", state->lookahead);
        return true;
    }

    return false;
}

static void scan(struct lr_parser_state *state) {
    state->lookahead = nfa_match(&state->match);
}

static void debug_parser_state(struct array *states, struct lr_parser_state *state) {
#define DEBUG_BUF 31
    if (!debug_is("gram_lr")) return;

    char input[DEBUG_BUF];
    gram_state_no st;
    debug("(");
    for (unsigned i = 0, ssize = asize(states); i < ssize; i++) {
        at(&st, i, states);
        debug("%u ", st);
    }
    debug(", %u, ", state->lookahead);

    char *in = state->match.match_start;
    int i;
    for (i = 0; i < DEBUG_BUF && in[i]; i++) {
        if (in[i] == '\n') input[i] = ' ';
        else input[i] = in[i];
    }
    input[i] = '\0';
    debug("\"%s\"", input);
    debug(")\n");
#undef DEBUG_BUF
}

#define STATES_STACK_SIZE 7
bool lr_parse(struct lr_error *error, char *input, struct lr_parser_state *state) {
    assert(state != NULL);

    if (!start_scanning(input, state)) return oom_error(error, NULL);

    struct array *states = init_array(sizeof (gram_state_no), STATES_STACK_SIZE, 0, 0);
    if (!states) return oom_error(error, NULL);

    struct lr_parser const *parser = state->parser;
    struct lr_action **atable = parser->atable;

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

        struct lr_action act = atable[s][state->lookahead];

        debug("action: %c(%u)\n", action_sym(act.action), act.n);

        if (act.action == GM_LR_SHIFT) {
            apush(&act.n, states);
            scan(state);
        } else if (act.action == GM_LR_REDUCE) {
            while (act.n) apop(&s, states), act.n--;
            apeek(&s, states);
            printf("parsed %u\n", act.nt);
            act = atable[s][act.nt];
            debug("action: %c(%u)\n", action_sym(act.action), act.n);
            apush(&act.n, states);
        } else if (act.action == GM_LR_ACCEPT) {
            break;
        } else success = syntax_error(error, 0, state);
            // FIXME: get the set of expected symbols at this point

        debug("\n");
    }


    free_array(states);

    return success;
}

void free_lr_parser_state(struct lr_parser_state *state) {
    free_nfa_match(&state->match);
}

#define ERROR_FMT_END "\n|\n"
#define OOM_ERROR_FMT_START "| LR Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
#define NOT_LR_FMT "| Grammar Not LR\n|\n"
#define SYNTAX_ERROR_FMT_START "| LR Syntax Error\n|\n| Got: %u\n| Expected: %u"
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
void print_lr_error(FILE *handle, struct lr_error error) {
    switch (error.type) {
        case GM_LR_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, error.actual, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_LR_NOT_SLR_ERROR:
            fprintf(handle, NOT_LR_FMT);
            break;
        case GM_LR_SCANNER_ERROR:
            print_regex_error(handle, error.scanerr);
            break;
        case GM_LR_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
    }
}

