#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <base/array.h>
#include <base/base.h>
#include <base/assert.h>
#include <base/debug.h>
#include <regex/nfa.h>
#include "gram/analyze.h"
#include "gram/lr.h"
#include "gram/states.h"
#include "gram/spec.h"

#include "internal/assert.c"
#include "internal/gen.c"
#include "internal/macros.c"
#include "internal/spec.c"

#define debug(...) debug_ns("gram_lr", __VA_ARGS__);

static bool _oom_error(struct lr_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfreel(p, args);
    va_end(args);

    prod(error, ((struct lr_error) { .type = GM_LR_OOM_ERROR, .file = file, .col = col }));

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

static bool not_slr_error(struct lr_error *error) {
    prod(error, ((struct lr_error) { .type = GM_LR_NOT_SLR_ERROR }));
    return false;
}

static bool not_lalr_error(struct lr_error *error) {
    prod(error, ((struct lr_error) { .type = GM_LR_NOT_LALR_ERROR }));
    return false;
}

static bool not_lr1_error(struct lr_error *error) {
    prod(error, ((struct lr_error) { .type = GM_LR_NOT_LR1_ERROR }));
    return false;
}

static bool scanner_error(struct lr_error *error, struct regex_error scanerr) {
    prod(error, ((struct lr_error) { .type = GM_LR_SCANNER_ERROR, .scanerr = scanerr }));
    return false;
}

static gram_sym_no *next_set(gram_state_no current_state, struct lr_parser const *parser) {
    gram_sym_no **next_sets = parser->next_sets;

    if (!next_sets[current_state]) {
        unsigned nterms = 0;
        struct lr_action const *row = parser->atable[current_state];
        FOR_TERM(parser->stats, s) nterms += row[s].action ? 1 : 0;
        gram_sym_no *next = calloc(nullterm(nterms), sizeof *next), *n = next;
        if (!next) abort();
        FOR_TERM(parser->stats, s) if (row[s].action) *n++ = s;
        next_sets[current_state] = next;
    }

    return next_sets[current_state];
}

static bool syntax_error(struct lr_error *error, gram_state_no current_state, struct lr_parser_state *state) {
    prod(error, ((struct lr_error) {
        .type = GM_LR_SYNTAX_ERROR,
        .loc = nfa_match_loc(&state->match),
        .symtab = state->parser->symtab,
        .actual = state->lookahead,
        .expected = next_set(current_state, state->parser)
    }));

    return false;
}

struct lr_parser lr_parser(
    unsigned const nstates, struct lr_action const **atable, struct lr_rule const *rtable,
    char **symtab, gram_sym_no **next_sets,
    struct nfa_context scanner, struct gram_stats const stats
) {
    return (struct lr_parser) { nstates, atable, rtable, symtab, next_sets, scanner, stats };
}

static gram_sym_no **alloc_next_sets(unsigned nstates) {
    return calloc(nstates, sizeof (gram_sym_no *));
}

static struct lr_rule *rule_table(struct gram_parser_spec const *spec) {
    struct lr_rule *rtable = calloc(offs(spec->stats.rules), sizeof *rtable);
    if (!rtable) return NULL;

    struct gram_symbol *nt = gram_nonterm0(spec);
    while (!gram_symbol_null(nt)) {
        gram_rule_no *r = nt->derives;
        while (*r) {
            rtable[*r] = (struct lr_rule) { nt->num, rulesize(spec->rules[*r]) };
            r++;
        }
        nt++;
    }

    return rtable;
}

#define ACCEPT() (struct lr_action) { GM_LR_ACCEPT }
#define SHIFT(num) (struct lr_action) { GM_LR_SHIFT, (num) }
#define REDUCE(num) (struct lr_action) { GM_LR_REDUCE, (num) }
#define GOTO(num) (struct lr_action) { GM_LR_GOTO, (num) }
static struct lr_action const **make_action_table(
    enum gram_class clas, struct lr_error *error, unsigned *nstates, struct lr_rule const *rtable,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san,
    struct gram_parser_spec const *spec
) {
    enum lr_item_type item_type = GM_LR0_ITEMS;
    bool (*errfn)(struct lr_error *error) = not_slr_error;

    if (clas == GM_LR1) {
        item_type = GM_LR1_ITEMS;
        errfn = not_lr1_error;
    } else if (clas == GM_LALR) {
        item_type = GM_LALR_ITEMS;
        errfn = not_lalr_error;
    }

    if (gan->clas < clas) return errfn(error), NULL;

    unsigned _nstates = 0;
    struct lr_state *states = NULL;
    if ((states = discover_lr_states(&_nstates, item_type, san, spec)) == NULL)
        return oom_error(error, NULL), NULL;

    struct gram_stats const stats = spec->stats;
    unsigned nsymbols = offs(stats.symbols);
    size_t atsize = _nstates * sizeof (struct lr_action *) + _nstates * nsymbols * sizeof (struct lr_action);
    struct lr_action **atable = malloc(atsize);
    if (!atable) return oom_error(error, NULL), free_lr_states(_nstates, states), NULL;

    struct array *stack = init_array(sizeof (struct lr_state *), 7, 0, 0);
    if (!stack) return oom_error(error, atable), free_lr_states(_nstates, states), NULL;

    memset(atable, 0, atsize);
    struct lr_action *rows = (struct lr_action *) (atable + _nstates);

    gram_sym_no const nonterm0 = offs(stats.terms);
    struct lr_state *state = states;

    apush(&state, stack);
    while (!aempty(stack)) {
        apop(&state, stack);

        if (atable[state->num]) continue;

        struct lr_action *row = rows + state->num * nsymbols;
        atable[state->num] = row;

        // reductions
        struct lr_itemset *itemset = state->itemset;
        for (unsigned i = 0; i < itemset->nitems; i++) {
            struct lr_item item = itemset->items[i];
            gram_sym_no *rule = spec->rules[item.rule];

            if (item.rule != GM_START && !rule[item.pos]) {
                struct lr_action act = REDUCE(item.rule);

                if (clas == GM_SLR) {
                    struct bsiter it = bsiter(san->follows[rtable[item.rule].nt]);
                    gram_sym_no s;
                    while (bsnext(&s, &it)) {
                        invariant(action_table_conflict, row, act, s, state->num);
                        row[s] = act;
                    }
                } else {
                    invariant(action_table_conflict, row, act, item.sym, state->num);
                    row[item.sym] = act;
                }
            }
        }

        // shifts, gotos, accept
        struct lr_transitions *trans = state->trans;
        for (unsigned i = 0; i < trans->nstates; i++) {
            struct lr_state *next = trans->states[i];

            struct lr_action act;
            if (next->sym == GM_EOF)       act = ACCEPT();
            else if (next->sym < nonterm0) act = SHIFT(next->num);
            else                           act = GOTO(next->num);

            invariant(action_table_conflict, row, act, next->sym, state->num);
            row[next->sym] = act;

            apush(&next, stack);
        }
    }

    free_lr_states(_nstates, states);
    free_array(stack);

    *nstates = _nstates;

    return (struct lr_action const **) atable;
}

struct lr_action const **slr_table(
    struct lr_error *error, unsigned *nstates, struct lr_rule const *rtable,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san,
    struct gram_parser_spec const *spec
) {
    debug("making slr table\n");
    return make_action_table(GM_SLR, error, nstates, rtable, gan, san, spec);
}

struct lr_action const **lalr_table(
    struct lr_error *error, unsigned *nstates, struct lr_rule const *rtable,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san,
    struct gram_parser_spec const *spec
) {
    debug("making lalr table\n");
    return make_action_table(GM_LALR, error, nstates, rtable, gan, san, spec);
}

struct lr_action const **lr1_table(
    struct lr_error *error, unsigned *nstates, struct lr_rule const *rtable,
    struct gram_analysis const *gan, struct gram_symbol_analysis const *san,
    struct gram_parser_spec const *spec
) {
    debug("making lr1 table\n");
    return make_action_table(GM_LR1, error, nstates, rtable, gan, san, spec);
}

bool gen_lr(
    struct lr_error *error, struct lr_parser *parser, action_table *table,
    struct gram_parser_spec *spec
) {
    gram_count(spec);
    invariant(assert_packed_spec, spec);
    assert(parser != NULL);
    assert(table != NULL);

    prod(error, ((struct lr_error) { 0 }));
    *parser = (struct lr_parser) { 0 };

    struct gram_stats stats = spec->stats;

    struct gram_symbol_analysis san = { 0 };
    struct gram_analysis gan = { 0 };
    struct nfa_context scanner = { 0 };
    struct lr_rule const *rtable = NULL;
    struct lr_action const **atable = NULL;
    char **symtab = NULL;
    gram_sym_no **next_sets = NULL;

    if (!gram_analyze_symbols(&san, spec))
        return oom_error(error, NULL);

    if (!gram_analyze(&gan, &san, spec)) {
        oom_error(error, NULL);
        goto free;
    }

    if (!init_scanner(&scanner, spec->patterns)) {
        scanner_error(error, nfa_error(&scanner));
        goto free;
    }

    if ((rtable = rule_table(spec)) == NULL) {
        oom_error(error, NULL);
        goto free;
    }

    unsigned nstates = 0;
    if ((atable = (*table)(error, &nstates, rtable, &gan, &san, spec)) == NULL)
        goto free;

    if ((symtab = gram_symbol_strings(spec)) == NULL)
        goto free;

    if ((next_sets = alloc_next_sets(nstates)) == NULL)
        goto free;

    free_gram_analysis(&gan);
    free_gram_symbol_analysis(&san);

    *parser = lr_parser(nstates, atable, rtable, symtab, next_sets, scanner, stats);

    return true;
free:
    free_gram_symbol_analysis(&san);
    free_gram_analysis(&gan);
    free_nfa_context(&scanner);
    freec(atable);
    freec(rtable);
    free(symtab);
    free(next_sets);

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
    if (act.action == GM_LR_ACCEPT) fmt = "\"%c\"";
    fprintf(handle, fmt, action_sym(act.action), act.n);
}

void print_lr_parser(FILE *handle, struct lr_parser const *parser) {
    assert(parser != NULL);

    struct lr_action const **atable = parser->atable;

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

static void free_next_sets(unsigned nstates, gram_sym_no **next_sets) {
    if (!next_sets) return;
    for (unsigned st = 0; st < nstates; st++)
        if (next_sets[st]) free(next_sets[st]);
    free(next_sets);
}

void free_lr_parser(struct lr_parser *parser) {
    if (!parser) return;
    freec(parser->atable);
    freec(parser->rtable);
    free_symtab(parser->symtab, parser->stats);
    free_next_sets(parser->nstates, parser->next_sets);
    free_nfa_context(&parser->scanner);
    *parser = (struct lr_parser) { 0 };
}

struct lr_parser_state lr_parser_state(struct lr_parser const *parser) {
    assert(parser != NULL);
    return (struct lr_parser_state) { .parser = (struct lr_parser *) parser };
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
    struct lr_action const **atable = parser->atable;
    struct lr_rule const *rtable = parser->rtable;

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
            struct lr_rule rule = rtable[act.n];
            while (rule.n) apop(&s, states), rule.n--;
            apeek(&s, states);
            printf("parsed %u\n", rule.nt);
            act = atable[s][rule.nt];
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

static void print_next_set(FILE *handle, char **symtab, gram_sym_no *next_set) {
    gram_sym_no *s = next_set;
    if (*s) {
        if (symtab) fprintf(handle, "%s", symtab[*s++]);
        else fprintf(handle, "%u", *s++);
        while (*s) {
            if (symtab) fprintf(handle, ", %s", symtab[*s++]);
            else fprintf(handle, ", %u", *s++);
        }
    }
}

#define ERROR_FMT_END "\n|\n"
#define SYNTAX_ERROR_FMT_START "| LR Syntax Error\n|\n| Got: %u\n| Expected: "
#define SYNTAX_ERROR_FMT_LOC "\n\n|\n| At: "
static void print_syntax_error(FILE *handle, struct lr_error error) {
    fprintf(handle, SYNTAX_ERROR_FMT_START, error.actual);
    print_next_set(handle, error.symtab, error.expected);
    fprintf(handle, SYNTAX_ERROR_FMT_LOC);
    print_regex_loc(handle, error.loc);
    fprintf(handle, ERROR_FMT_END);
}

#define OOM_ERROR_FMT_START "| LR Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
#define NOT_SLR_FMT "| Grammar Not SLR\n|\n"
#define NOT_LALR_FMT "| Grammar Not LALR\n|\n"
#define NOT_LR1_FMT "| Grammar Not LR1\n|\n"
void print_lr_error(FILE *handle, struct lr_error error) {
    switch (error.type) {
        case GM_LR_SYNTAX_ERROR:
            print_syntax_error(handle, error);
            break;
        case GM_LR_NOT_SLR_ERROR:
            fprintf(handle, NOT_SLR_FMT);
            break;
        case GM_LR_NOT_LALR_ERROR:
            fprintf(handle, NOT_LALR_FMT);
            break;
        case GM_LR_NOT_LR1_ERROR:
            fprintf(handle, NOT_LR1_FMT);
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

