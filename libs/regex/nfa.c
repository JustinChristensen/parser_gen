#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <base/list.h>
#include <base/debug.h>
#include <base/string.h>
#include "regex/nfa.h"
#include "regex/run_parser.h"
#include "regex/result_types.h"

#define ndebug(...) debug_ns_("nfa", __VA_ARGS__);

bool (*nfa_actions[])(union regex_result val, void *context) = {
    [AI(DO_REGEX)] =        ACTION noop_nfa,
    [AI(DO_EMPTY)] =        ACTION do_empty_nfa,
    [AI(DO_ALT)] =          ACTION do_alt_nfa,
    [AI(DO_CAT)] =          ACTION do_cat_nfa,
    [AI(DO_SUB)] =          ACTION noop_nfa,
    [AI(DO_TAG)] =          ACTION do_tag_nfa,
    [AI(DO_CHAR_CLASS)] =   ACTION do_class_nfa,
    [AI(DO_NEG_CLASS)] =    ACTION do_neg_class_nfa,
    [AI(DO_DOTALL)] =       ACTION do_dotall_nfa,
    [AI(DO_SYMBOL)] =       ACTION do_symbol_nfa,
    [AI(DO_RANGES)] =       ACTION do_range_nfa,
    [AI(DO_RANGE)] =        ACTION do_range_nfa,
    [AI(DO_STAR)] =         ACTION do_star_nfa,
    [AI(DO_PLUS)] =         ACTION do_plus_nfa,
    [AI(DO_OPTIONAL)] =     ACTION do_optional_nfa,
    [AI(DO_REPEAT_EXACT)] = ACTION do_repeat_exact_nfa
};

struct parse_interface nfa_pinterface = {
    .result = RESULTFN nfa_to_result,
    .has_error = HASERRFN nfa_has_error,
    .error = ERRFN nfa_error
};

static void nfa_oom(struct nfa_context *context) {
    context->has_error = true;
    context->error = oom_error();
}

static void debug_class_state(struct nfa_state *state) {
    bool *char_class = state->char_class;
    debug_("[");
    for (int i = 0; i < CLASS_SIZE; i++)
        debug_(char_class[i] ? "1" : "0");
    debug_("]");
}

static void debug_state(struct nfa_state *state) {
    debug_("(%d, ", state->id);
    switch (state->type) {
        case ACCEPTING_STATE:
            debug_("accept");
            break;
        case EPSILON_STATE:
            debug_("eps, %p", state->next);
            break;
        case DOTALL_STATE:
            debug_("dotall, %p", state->next);
            break;
        case BRANCH_STATE:
            debug_("branch, %p, %p", state->left, state->right);
            break;
        case CLASS_STATE:
            debug_("class, %p, ", state->next);
            debug_class_state(state);
            break;
        case SYMBOL_STATE:
            debug_("symbol, %p, %c", state->next, state->symbol);
            break;
    }
    debug_(")");
}

static void debug_nfa_states(struct list *cstates) {
    struct node *node;

    if (!empty(cstates)) {
        ndebug("{");
        node = head(cstates);
        debug_state(value(node));
        for (node = node->next; node; node = node->next) {
            debug_(", ");
            debug_state(value(node));
        }
        debug_("}");
    } else {
        ndebug("empty");
    }

    debug_("\n");
}

static void debug_state_table(struct nfa_state_pool *pool) {
    ndebug("nfa state table\n");

    for (; pool; pool = pool->next) {
        for (int i = 0; i < pool->n; i++) {
            struct nfa_state *state = &pool->states[i];
            ndebug("%p: ", state);
            debug_state(state);
            debug_("\n");
        }
    }
}

static void debug_nfa(struct nfa mach) {
    if (mach.start) {
        ndebug("nfa { start: %p", mach.start);
        if (mach.end) debug_(", end: %p -> %p", mach.end, *mach.end);
        if (mach.end1) debug_(", end1: %p -> %p", mach.end1, *mach.end1);
        debug_(" }\n");
    }
}

static struct nfa_state_pool *state_pool() {
    struct nfa_state_pool *pool = malloc(sizeof *pool);

    if (pool) {
        *pool = (struct nfa_state_pool) { .next = NULL, .n = 0 };
    }

    return pool;
}

static struct nfa_state_pool *next_state_pool(struct nfa_state_pool *prev) {
    prev->next = state_pool();
    return prev->next;
}

struct nfa_context nfa_context(struct regex_pattern const *pat, bool use_nonrec) {
    struct nfa_context context = { .use_nonrec = use_nonrec };

    if ((context.state_pool = state_pool())) {
        context.state_pools = context.state_pool;

        if (pat) {
            while (!is_null_pattern(pat)) {
                nfa_regex(pat->sym, pat->tag, pat->pattern, &context);
                pat++;
            }
        }
    } else nfa_oom(&context);

    return context;
}

struct nfa_state accepting_state(int sym) {
    return (struct nfa_state) {
        .type = ACCEPTING_STATE,
        .sym = sym
    };
}

struct nfa_state epsilon_state(struct nfa_state *next) {
    return (struct nfa_state) {
        .type = EPSILON_STATE,
        .next = next
    };
}

struct nfa_state dotall_state(struct nfa_state *next) {
    return (struct nfa_state) {
        .type = DOTALL_STATE,
        .next = next
    };
}

struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right) {
    return (struct nfa_state) {
        .type = BRANCH_STATE,
        .left = left,
        .right = right
    };
}

struct nfa_state class_state(bool *char_class) {
    return (struct nfa_state) {
        .type = CLASS_STATE,
        .char_class = char_class,
        .next = NULL
    };
}

struct nfa_state symbol_state(char sym) {
    return (struct nfa_state) {
        .type = SYMBOL_STATE,
        .symbol = sym,
        .next = NULL
    };
}

struct nfa_state *setst(struct nfa_state state, struct nfa_context *context) {
    struct nfa_state_pool *pool = context->state_pool;
    struct nfa_state *statep = NULL;

    if (pool->n == STATE_POOL_SIZE) {
        pool = context->state_pool = next_state_pool(context->state_pool);
    }

    if (pool) {
        state.id = context->num_states++;
        pool->states[pool->n] = state;
        statep = &pool->states[pool->n];
        pool->n++;
    } else nfa_oom(context);

    return statep;
}

void dangle(struct nfa *machine, struct nfa_state **end, struct nfa_state **end1) {
    machine->end = end;
    machine->end1 = end1;
}

// TODO: merge operation
// - if the machine has two end pointers
//      - creates an epsilon state and points the ends to it
// - otherwise does nothing

void point(struct nfa machine, struct nfa_state *state) {
    *machine.end = state;
    if (machine.end1) *machine.end1 = *machine.end;
}

void smachine(struct nfa machine, struct nfa_context *context) {
    context->nfa = machine;
}

struct nfa gmachine(struct nfa_context *context) {
    return context->nfa;
}

union regex_result nfa_to_result(struct nfa_context *context) {
    return (union regex_result) { .mach = gmachine(context) };
}

struct nfa empty_machine(struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(epsilon_state(NULL), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa dotall_machine(struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(dotall_state(NULL), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa class_machine(struct nfa_context *context, bool *char_class) {
    struct nfa machine;
    machine.start = setst(class_state(char_class), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa symbol_machine(struct nfa_context *context, char sym) {
    struct nfa machine;
    machine.start = setst(symbol_state(sym), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

struct nfa alt_machine(struct nfa_context *context, struct nfa left, struct nfa right) {
    struct nfa machine;
    machine.start = setst(branch_state(left.start, right.start), context);
    // TODO: we only need an epsilon state here if the machine has two end pointers
    point(left, setst(epsilon_state(NULL), context));
    point(right, setst(epsilon_state(NULL), context));
    dangle(&machine, &(*left.end)->next, &(*right.end)->next);
    return machine;
}

struct nfa cat_machine(struct nfa first, struct nfa second) {
    struct nfa machine;
    point(first, second.start);
    machine.start = first.start;
    dangle(&machine, second.end, second.end1);
    return machine;
}

struct nfa posclosure_machine(struct nfa_context *context, struct nfa inner) {
    struct nfa machine;
    machine.start = inner.start;
    point(inner, setst(branch_state(inner.start, NULL), context));
    dangle(&machine, &(*inner.end)->right, NULL);
    return machine;
}

struct nfa optional_machine(struct nfa_context *context, struct nfa inner) {
    struct nfa machine;
    point(inner, setst(epsilon_state(NULL), context));
    machine.start = setst(branch_state(inner.start, NULL), context);
    dangle(&machine, &(*inner.end)->next, &machine.start->right);
    return machine;
}

struct nfa closure_machine(struct nfa_context *context, struct nfa inner) {
    return optional_machine(context, posclosure_machine(context, inner));
}

bool nfa_regex(int sym, char *tag, char *pattern, struct nfa_context *context) {
    if (!nfa_has_error(context)) {
        struct parse_context pcontext = parse_context(context, nfa_pinterface, nfa_actions, context->use_nonrec);

        if (run_parser(pattern, &pcontext)) {
            point(gmachine(context), setst(accepting_state(sym), context));

            if (!tag) return true;

            if ((tag = strdup(tag))) {
                debug_state_table(context->state_pools);
                debug_nfa(gmachine(context));
                return tag_machine(tag, context);
            } else nfa_oom(context);
        } else {
            context->has_error = pcontext.has_error;
            context->error = pcontext.error;
        }
    }

    return false;
}

bool nfa_has_error(struct nfa_context *context) {
    return context->has_error;
}

struct regex_error nfa_error(struct nfa_context *context) {
    return context->error;
}

static void free_nfa_states(struct nfa_state *states, int n) {
    if (!states) return;

    for (int i = 0; i < n; i++) {
        struct nfa_state state = states[i];

        switch (state.type) {
            case CLASS_STATE:
                free(state.char_class);
                state.char_class = NULL;
                break;
            default:
                break;
        }
    }
}

static void free_pools(struct nfa_context *context) {
    struct nfa_state_pool *pool = NULL, *next = NULL;

    for (pool = context->state_pools; pool != NULL; pool = next) {
        next = pool->next;
        free_nfa_states(pool->states, pool->n);
        free(pool);
    }

    context->state_pools = context->state_pool = NULL;
}

static void free_tagged_nfas(struct nfa_context *context) {
    struct tagged_nfa *tnfa = NULL, *next = NULL;

    for (tnfa = context->tagged_nfas; tnfa != NULL; tnfa = next) {
        next = tnfa->next;
        free(tnfa->tag);
        tnfa->tag = NULL;
        free(tnfa);
    }

    context->tagged_nfas = NULL;
}

void free_nfa_context(struct nfa_context *context) {
    if (!context) return;
    free_pools(context);
    free_tagged_nfas(context);
}

void eps_closure(struct list *nstates, struct nfa_state *state, bool *already_on) {
    if (already_on[state->id]) return;

    push(nstates, state);
    already_on[state->id] = true;

    switch (state->type) {
        case EPSILON_STATE:
            eps_closure(nstates, state->next, already_on);
            break;
        case BRANCH_STATE:
            eps_closure(nstates, state->left, already_on);
            eps_closure(nstates, state->right, already_on);
            break;
        case ACCEPTING_STATE:
        case DOTALL_STATE:
        case CLASS_STATE:
        case SYMBOL_STATE:
            break;
    }
}

// Char -> State -> Bool
static bool matches_state(unsigned char c, struct nfa_state *state) {
    // state as a predicate function
    switch (state->type) {
        case DOTALL_STATE:
            return true;
        case SYMBOL_STATE:
            return state->symbol == c;
        case CLASS_STATE:
            return state->char_class[c];
        default:
            return false;
    }
}

void move(struct list *nstates, struct list *cstates, char c, bool *already_on) {
    struct node *node;
    struct nfa_state *state;
    while ((node = pop(cstates)) && (state = value(node))) {
        if (matches_state(c, state)) {
            eps_closure(nstates, state->next, already_on);
        }

        free_node(node, NULL);
    }
}

bool accepts(struct list *cstates, struct nfa_state *accept) {
    struct node *node, *next;
    struct nfa_state *state;

    for (node = head(cstates); node; node = next) {
        state = value(node);

        if (state == accept) {
            return true;
        }

        next = node->next;
    }

    return false;
}

struct nfa_match nfa_match_state(char *input, struct nfa_context *context) {
    return (struct nfa_match) {
        .context = context,
        .input = input,
        .input_loc = regex_loc(0, 0)
    };
}

struct regex_loc nfa_match_loc(struct nfa_match *match) {
    return match->match_loc;
}

void nfa_match_lexeme(char *lexeme, struct nfa_match *match) {
    strncpy(lexeme, match->match_start, match->input - match->match_start);
}

int nfa_match(struct nfa_match *match) {
    assert(match != NULL);

    struct nfa_context *context = match->context;
    struct nfa nfa = context->nfa;
    size_t num_states = context->num_states;
    bool *already_on = calloc(num_states, sizeof *already_on);
    struct list *cstates = init_list();
    struct list *nstates = init_list();
    char *input = match->input;
    struct regex_loc input_loc = match->input_loc;
    int foundsym = 0;

    match->match_start = input;
    match->match_loc = input_loc;

    eps_closure(cstates, nfa.start, already_on);

    ndebug("nfa simulation\n");
    debug_nfa_states(cstates);
    char c;
    struct list *t;
    while (!empty(cstates) && (c = *input++) != '\0') {
        if (c == '\n') {
            input_loc.line++;
            input_loc.col = 1;
        } else {
            input_loc.col++;
        }

        memset(already_on, false, num_states);
        move(nstates, cstates, c, already_on);
        t = cstates;
        cstates = nstates;
        nstates = t;
        debug_nfa_states(cstates);
    }

    match->input = input;
    match->input_loc = input_loc;

    struct nfa_state *accstate = *nfa.end;

    if (accepts(cstates, accstate))
        foundsym = accstate->sym;

    free(already_on);
    free_list(nstates, NULL);
    free_list(cstates, NULL);

    return foundsym;
}

static void add_pointer(struct nfa *mach, struct nfa_state **end) {
    if (mach->end) {
        mach->end1 = end;
    } else {
        mach->end = end;
    }
}

static bool endptr(struct nfa_state **p, struct nfa *mach) {
    return mach->end == p || (mach->end1 && mach->end1 == p);
}

static struct nfa_state *_clone_machine(
    struct nfa_state **visited,
    struct nfa *newmach,
    struct nfa_state *state,
    struct nfa *oldmach,
    struct nfa_context *context
) {
    if (visited[state->id]) return visited[state->id];

    struct nfa_state *nextstate;

    nextstate = visited[state->id] = setst(*state, context);

    // in theory class states could share, but that makes certain things
    // more difficult, so I'll add state state sharing to the wishlist
    if (state->type == CLASS_STATE) {
        bool *char_class = calloc(CLASS_SIZE, sizeof *char_class);

        if (char_class) {
            memcpy(char_class, state->char_class, CLASS_SIZE);
            nextstate->char_class = char_class;
        } else {
            nfa_oom(context);
            return NULL;
        }
    }

    switch (state->type) {
        case BRANCH_STATE:
            if (!endptr(&state->left, oldmach))
                nextstate->left = _clone_machine(visited, newmach, state->left, oldmach, context);
            else {
                nextstate->left = NULL;
                add_pointer(newmach, &nextstate->left);
            }

            if (!endptr(&state->right, oldmach))
                nextstate->right = _clone_machine(visited, newmach, state->right, oldmach, context);
            else {
                nextstate->right = NULL;
                add_pointer(newmach, &nextstate->right);
            }
            break;
        case CLASS_STATE:
        case SYMBOL_STATE:
        case DOTALL_STATE:
        case EPSILON_STATE:
            if (!endptr(&state->next, oldmach))
                nextstate->next = _clone_machine(visited, newmach, state->next, oldmach, context);
            else {
                nextstate->next = NULL;
                add_pointer(newmach, &nextstate->next);
            }
            break;
        case ACCEPTING_STATE:
            break;
    }

    return nextstate;
}

bool clone_machine(struct nfa mach, struct nfa_context *context) {
    struct nfa newmach = { NULL, NULL, NULL };
    struct nfa_state **visited = calloc(context->num_states, sizeof *visited);

    if (!visited) {
        nfa_oom(context);
        return false;
    }

    newmach.start = _clone_machine(visited, &newmach, mach.start, &mach, context);
    free(visited);

    if (newmach.start) {
        smachine(newmach, context);
        return true;
    }

    return false;
}

struct tagged_nfa *find_machine(char *tag, struct nfa_context *context) {
    struct tagged_nfa *tnfa = context->tagged_nfas;

    for (; tnfa; tnfa = tnfa->next) {
        if (streq(tag, tnfa->tag))
            return tnfa;
    }

    return NULL;
}

bool tag_machine(char *tag, struct nfa_context *context) {
    struct tagged_nfa *tnfa = context->tagged_nfas,
                      *last = NULL;
    bool found = false;

    for (; tnfa; tnfa = tnfa->next) {
        last = tnfa;

        if (streq(tag, tnfa->tag)) {
            found = true;
            break;
        }
    }

    if (found) {
        tnfa->nfa = gmachine(context);
        return true;
    }

    if ((tnfa = malloc(sizeof *tnfa)) == NULL) {
        nfa_oom(context);
        return false;
    }

    *tnfa = (struct tagged_nfa) {
        .tag = tag,
        .nfa = gmachine(context)
    };

    if (last) last->next = tnfa;
    else      context->tagged_nfas = tnfa;

    return true;
}

bool noop_nfa(union regex_result _, struct nfa_context *context) { return true; }

bool do_tag_nfa(union regex_result tag, struct nfa_context *context) {
    struct tagged_nfa *tnfa = find_machine(tag.tag, context);

    if (tnfa) {
        clone_machine(tnfa->nfa, context);
        return true;
    }

    context->has_error = true;
    context->error = missing_tag_error(tag.tag);

    return false;
}

bool do_empty_nfa(union regex_result _, struct nfa_context *context) {
    smachine(empty_machine(context), context);
    return true;
}

bool do_alt_nfa(union regex_result nfa, struct nfa_context *context) {
    smachine(alt_machine(context, nfa.mach, gmachine(context)), context);
    return true;
}

bool do_cat_nfa(union regex_result nfa, struct nfa_context *context) {
    smachine(cat_machine(nfa.mach, gmachine(context)), context);
    return true;
}

bool do_dotall_nfa(union regex_result _, struct nfa_context *context) {
    smachine(dotall_machine(context), context);
    return true;
}

bool do_symbol_nfa(union regex_result sym, struct nfa_context *context) {
    smachine(symbol_machine(context, sym.tval.sym), context);
    return true;
}

static bool *get_classy(struct nfa_context *context) {
    bool *char_class = context->current_class;

    // allocate a new in-progress character class
    if (!char_class) {
        // prioritizing time and implementation complexity over space
        // i.e. this could be a table of bitmaps to conserve bytes
        char_class = calloc(CLASS_SIZE, sizeof *char_class);
        if (!char_class) nfa_oom(context);
    }

    context->current_class = char_class;

    return char_class;
}

bool do_range_nfa(union regex_result range, struct nfa_context *context) {
    bool *char_class;

    if ((char_class = get_classy(context))) {
        struct char_range r = range.tval.range;
        for (unsigned char s = r.start, e = r.end; s <= e; s++)
            char_class[s] = true;
        return true;
    }

    return false;
}

bool do_class_nfa(union regex_result _, struct nfa_context *context) {
    bool *char_class;

    if ((char_class = get_classy(context))) {
        smachine(class_machine(context, char_class), context);
        context->current_class = NULL;
        return true;
    }

    return false;
}

bool do_neg_class_nfa(union regex_result _, struct nfa_context *context) {
    bool *char_class = gmachine(context).start->char_class;
    if (char_class) {
        for (int i = 0; i < CLASS_SIZE; i++) char_class[i] = !char_class[i];
    }
    return true;
}

bool do_star_nfa(union regex_result _, struct nfa_context *context) {
    smachine(closure_machine(context, gmachine(context)), context);
    return true;
}

bool do_plus_nfa(union regex_result _, struct nfa_context *context) {
    smachine(posclosure_machine(context, gmachine(context)), context);
    return true;
}

bool do_optional_nfa(union regex_result _, struct nfa_context *context) {
    smachine(optional_machine(context, gmachine(context)), context);
    return true;
}

bool do_repeat_exact_nfa(union regex_result num, struct nfa_context *context) {
    int n = num.tval.num;

    if (n > 0) {
        struct nfa lhs, orig = gmachine(context);
        bool success = true;

        ndebug("original machine\n");
        debug_nfa(orig);
        debug_state_table(context->state_pools);

        for (int i = 1; i < n && success; i++) {
            lhs = gmachine(context);
            clone_machine(orig, context);
            success = do_cat_nfa((union regex_result) { .mach = lhs }, context);
        }

        ndebug("cloned machine\n");
        debug_nfa(gmachine(context));
        debug_state_table(context->state_pools);

        return success;
    }

    context->has_error = true;
    context->error = repeat_zero_error();

    return false;
}

