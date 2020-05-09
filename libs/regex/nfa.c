#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include <assert.h>
#include <string.h>
#include <base/debug.h>
#include <base/string.h>
#include <base/macros.h>
#include <base/hash_table.h>
#include "regex/base.h"
#include "regex/nfa.h"
#include "parser.h"

#define debug(...) debug_ns("regex_nfa", __VA_ARGS__);
#define CLASS_SIZE (UCHAR_MAX + 1)

static void debug_class_state(struct nfa_state *state) {
    bool *char_class = state->char_class;
    debug("[");
    for (int i = 0; i < CLASS_SIZE; i++)
        debug(char_class[i] ? "1" : "0");
    debug("]");
}

static void debug_state(struct nfa_state *state) {
    debug("(%d, ", state->id);
    switch (state->type) {
        case RX_ACCEPTING_STATE:
            debug("accept, %d", state->sym);
            break;
        case RX_EPSILON_STATE:
            debug("eps, %p", state->next);
            break;
        case RX_DOTALL_STATE:
            debug("dotall, %p", state->next);
            break;
        case RX_BRANCH_STATE:
            debug("branch, %p, %p", state->left, state->right);
            break;
        case RX_CLASS_STATE:
            debug("class, %p, ", state->next);
            debug_class_state(state);
            break;
        case RX_CHAR_STATE:
            debug("char, %p, %c", state->next, state->ch);
            break;
    }
    debug(")");
}

static void debug_nfa_states(struct nfa_state **start, struct nfa_state **end) {
    if (start != end) {
        struct nfa_state *state = *start++;
        debug("{");
        debug_state(state);
        while (start != end && (state = *start)) {
            debug(", ");
            debug_state(state);
            start++;
        }
        debug("}");
    } else {
        debug("empty");
    }

    debug("\n");
}

static void debug_state_table(struct nfa_state_pool *pool) {
    debug("state table\n");

    for (; pool; pool = pool->next) {
        for (int i = 0; i < pool->n; i++) {
            struct nfa_state *state = &pool->states[i];
            debug("%p: ", state);
            debug_state(state);
            debug("\n");
        }
    }
}

static void debug_nfa(struct nfa mach) {
    if (mach.start) {
        debug("nfa { start: %p", mach.start);
        if (mach.end) debug(", end: %p -> %p", mach.end, *mach.end);
        if (mach.end1) debug(", end1: %p -> %p", mach.end1, *mach.end1);
        debug(" }");
    }
}

static void debug_nfa_p(FILE *_, void const *mach) {
    UNUSED(_);
    debug_nfa(*((struct nfa *) mach));
}

static void debug_tagged_nfas(struct hash_table *tagged_nfas) {
    debug("tagged nfas\n");
    if (debug_is("regex_nfa")) print_hash_entries(stderr, debug_nfa_p, tagged_nfas);
}

static void debug_pattern(int sym, char *pattern) {
    debug("pattern /%s/, sym %d\n", pattern, sym);
}

static bool set_oom_error(struct nfa_context *context) {
    if (!context->has_error) {
        debug("oom error\n");
        context->has_error = true;
        context->error = regex_oom_error();
    }

    return false;
}

static bool set_missing_tag_error(char *tag, struct nfa_context *context) {
    static char permatag[BUFSIZ] = "";
    if (!context->has_error) {
        debug("missing tag error\n");
        strcpy(permatag, tag);
        context->has_error = true;
        context->error = regex_missing_tag_error(permatag);
    }

    return false;
}

static bool set_tag_exists_error(char *tag, struct nfa_context *context) {
    static char permatag[BUFSIZ] = "";

    if (!context->has_error) {
        debug("tag exists error\n");
        strcpy(permatag, tag);
        context->has_error = true;
        context->error = regex_tag_exists_error(permatag);
    }

    return false;
}

static bool set_duplicate_pattern_error(char *pattern, struct nfa_context *context) {
    static char permapat[BUFSIZ] = "";

    if (!context->has_error) {
        debug("duplicate pattern error\n");
        strcpy(permapat, pattern);
        context->has_error = true;
        context->error = regex_duplicate_pattern_error(permapat);
    }

    return false;
}

static bool set_repeat_zero_error(struct nfa_context *context) {
    if (!context->has_error) {
        debug("repeat zero error\n");
        context->has_error = true;
        context->error = regex_repeat_zero_error();
    }

    return false;
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

static bool *get_classy(struct nfa_context *context) {
    bool *char_class = context->current_class;

    // allocate a new in-progress character class
    if (!char_class) {
        // prioritizing time and implementation complexity over space
        // i.e. this could be a table of bitmaps to conserve bytes
        char_class = calloc(CLASS_SIZE, sizeof *char_class);
        if (!char_class) set_oom_error(context);
    }

    context->current_class = char_class;

    return char_class;
}

static struct nfa_state accepting_state(int sym) {
    return (struct nfa_state) {
        .type = RX_ACCEPTING_STATE,
        .sym = sym
    };
}

static struct nfa_state epsilon_state(struct nfa_state *next) {
    return (struct nfa_state) {
        .type = RX_EPSILON_STATE,
        .next = next
    };
}

static struct nfa_state dotall_state(struct nfa_state *next) {
    return (struct nfa_state) {
        .type = RX_DOTALL_STATE,
        .next = next
    };
}

static struct nfa_state branch_state(struct nfa_state *left, struct nfa_state *right) {
    return (struct nfa_state) {
        .type = RX_BRANCH_STATE,
        .left = left,
        .right = right
    };
}

static struct nfa_state class_state(bool *char_class) {
    return (struct nfa_state) {
        .type = RX_CLASS_STATE,
        .char_class = char_class,
        .next = NULL
    };
}

static struct nfa_state char_state(char ch) {
    return (struct nfa_state) {
        .type = RX_CHAR_STATE,
        .ch = ch,
        .next = NULL
    };
}

static struct nfa_state *setst(struct nfa_state state, struct nfa_context *context) {
    struct nfa_state_pool *pool = context->state_pool;
    struct nfa_state *statep = NULL;

    if (pool->n == RX_STATE_POOL_SIZE) {
        pool = context->state_pool = next_state_pool(context->state_pool);
    }

    if (pool) {
        state.id = context->num_states++;
        pool->states[pool->n] = state;
        statep = &pool->states[pool->n];
        pool->n++;
    } else
        set_oom_error(context);

    return statep;
}

static void dangle(struct nfa *machine, struct nfa_state **end, struct nfa_state **end1) {
    machine->end = end;
    machine->end1 = end1;
}

static struct nfa_state **point(struct nfa machine, struct nfa_state *state) {
    *machine.end = state;
    if (machine.end1) *machine.end1 = *machine.end;
    return &(*machine.end)->next;
}

static struct nfa_state **merge(struct nfa machine, struct nfa_context *context) {
    struct nfa_state **end = machine.end;

    if (machine.end && machine.end1) {
        end = point(machine, setst(epsilon_state(NULL), context));
    }

    return end;
}

static void smachine(struct nfa machine, struct nfa_context *context) {
    context->nfa = machine;
}

struct nfa nfa_gmachine(struct nfa_context *context) {
    return context->nfa;
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
    if (state->type == RX_CLASS_STATE) {
        bool *char_class = calloc(CLASS_SIZE, sizeof *char_class);

        if (char_class) {
            memcpy(char_class, state->char_class, CLASS_SIZE);
            nextstate->char_class = char_class;
        } else {
            set_oom_error(context);
            return NULL;
        }
    }

    switch (state->type) {
        case RX_BRANCH_STATE:
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
        case RX_CLASS_STATE:
        case RX_CHAR_STATE:
        case RX_DOTALL_STATE:
        case RX_EPSILON_STATE:
            if (!endptr(&state->next, oldmach))
                nextstate->next = _clone_machine(visited, newmach, state->next, oldmach, context);
            else {
                nextstate->next = NULL;
                add_pointer(newmach, &nextstate->next);
            }
            break;
        case RX_ACCEPTING_STATE:
            break;
    }

    return nextstate;
}

static bool clone_machine(struct nfa mach, struct nfa_context *context) {
    struct nfa newmach = { NULL, NULL, NULL };
    struct nfa_state **visited = calloc(context->num_states, sizeof *visited);

    if (!visited) {
        return set_oom_error(context);
    }

    newmach.start = _clone_machine(visited, &newmach, mach.start, &mach, context);
    free(visited);

    if (newmach.start) {
        smachine(newmach, context);
        return true;
    }

    return false;
}

static struct nfa *find_machine(char *tag, struct nfa_context *context) {
    return htlookup(tag, context->tagged_nfas);
}

static bool tag_machine(char *tag, struct nfa_context *context) {
    if (htcontains(tag, context->tagged_nfas))
        return set_tag_exists_error(tag, context);
    struct nfa mach = nfa_gmachine(context);
    htinsert(tag, &mach, context->tagged_nfas);
    return true;
}

static union regex_result nfa_to_result(struct nfa_context *context) {
    return (union regex_result) { .mach = nfa_gmachine(context) };
}

static struct nfa empty_machine(struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(epsilon_state(NULL), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

static struct nfa dotall_machine(struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(dotall_state(NULL), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

static struct nfa class_machine(bool *char_class, struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(class_state(char_class), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

static struct nfa char_machine(char ch, struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(char_state(ch), context);
    dangle(&machine, &machine.start->next, NULL);
    return machine;
}

static struct nfa alt_machine(struct nfa left, struct nfa right, struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(branch_state(left.start, right.start), context);
    machine.end = merge(left, context);
    machine.end1 = merge(right, context);
    return machine;
}

static struct nfa cat_machine(struct nfa first, struct nfa second) {
    struct nfa machine;
    point(first, second.start);
    machine.start = first.start;
    dangle(&machine, second.end, second.end1);
    return machine;
}

static struct nfa posclosure_machine(struct nfa inner, struct nfa_context *context) {
    struct nfa machine;
    machine.start = inner.start;
    point(inner, setst(branch_state(inner.start, NULL), context));
    dangle(&machine, &(*inner.end)->right, NULL);
    return machine;
}

static struct nfa optional_machine(struct nfa inner, struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(branch_state(inner.start, NULL), context);
    machine.end = merge(inner, context);
    machine.end1 = &machine.start->right;
    return machine;
}

static struct nfa closure_machine(struct nfa inner, struct nfa_context *context) {
    return optional_machine(posclosure_machine(inner, context), context);
}

static void link_machines(struct nfa left, struct nfa right, struct nfa_context *context) {
    struct nfa machine;
    machine.start = setst(branch_state(left.start, right.start), context);
    machine.end = machine.end1 = NULL;
    smachine(machine, context);
}

static bool runnable(struct nfa machine) {
    return machine.start != NULL;
}

static bool noop_nfa(union regex_result _1, struct nfa_context *_2) { UNUSED(_1); UNUSED(_2); return true; }

static bool do_tag_nfa(union regex_result tag, struct nfa_context *context) {
    struct nfa *nfa = find_machine(tag.tag, context);

    if (nfa) {
        clone_machine(*nfa, context);
        return true;
    }

    return set_missing_tag_error(tag.tag, context);
}

static bool do_empty_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    smachine(empty_machine(context), context);
    return true;
}

static bool do_alt_nfa(union regex_result lhs, struct nfa_context *context) {
    smachine(alt_machine(lhs.mach, nfa_gmachine(context), context), context);
    return true;
}

static bool do_cat_nfa(union regex_result lhs, struct nfa_context *context) {
    smachine(cat_machine(lhs.mach, nfa_gmachine(context)), context);
    return true;
}

static bool do_dotall_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    smachine(dotall_machine(context), context);
    return true;
}

static bool do_char_nfa(union regex_result ch, struct nfa_context *context) {
    smachine(char_machine(ch.tval.ch, context), context);
    return true;
}

static bool do_range_nfa(union regex_result range, struct nfa_context *context) {
    bool *char_class;

    if ((char_class = get_classy(context))) {
        struct regex_char_range r = range.tval.range;
        for (unsigned char s = r.start, e = r.end; s <= e; s++)
            char_class[s] = true;
        return true;
    }

    return false;
}

static bool do_class_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    bool *char_class;

    if ((char_class = get_classy(context))) {
        smachine(class_machine(char_class, context), context);
        context->current_class = NULL;
        return true;
    }

    return false;
}

static bool do_neg_class_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    bool *char_class = nfa_gmachine(context).start->char_class;
    if (char_class) {
        for (int i = 0; i < CLASS_SIZE; i++) char_class[i] = !char_class[i];
    }
    return true;
}

static bool do_star_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    smachine(closure_machine(nfa_gmachine(context), context), context);
    return true;
}

static bool do_plus_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    smachine(posclosure_machine(nfa_gmachine(context), context), context);
    return true;
}

static bool do_optional_nfa(union regex_result _, struct nfa_context *context) {
    UNUSED(_);
    smachine(optional_machine(nfa_gmachine(context), context), context);
    return true;
}

static bool do_repeat_exact_nfa(union regex_result num, struct nfa_context *context) {
    int n = num.tval.num;

    if (n > 0) {
        struct nfa lhs, orig = nfa_gmachine(context);
        bool success = true;

        debug("original machine\n");
        debug_nfa(orig);
        debug_state_table(context->state_pools);

        for (int i = 1; i < n && success; i++) {
            lhs = nfa_gmachine(context);
            clone_machine(orig, context);
            success = do_cat_nfa((union regex_result) { .mach = lhs }, context);
        }

        debug("cloned machine\n");
        debug_nfa(nfa_gmachine(context));
        debug_state_table(context->state_pools);

        return success;
    }

    return set_repeat_zero_error(context);
}

static struct nfa nullmach() {
    return (struct nfa) { NULL, NULL, NULL };
}

static void reset_context(struct nfa_context *context) {
    *context = (struct nfa_context) {
        .nfa = nullmach(),
        .has_error = false,
        .error = regex_nullerror()
    };
}

static bool (*const nfa_actions[])(union regex_result val, struct nfa_context *context) = {
    [AI(RX_DO_REGEX)] =        noop_nfa,
    [AI(RX_DO_EMPTY)] =        do_empty_nfa,
    [AI(RX_DO_ALT)] =          do_alt_nfa,
    [AI(RX_DO_CAT)] =          do_cat_nfa,
    [AI(RX_DO_SUB)] =          noop_nfa,
    [AI(RX_DO_TAG)] =          do_tag_nfa,
    [AI(RX_DO_CHAR_CLASS)] =   do_class_nfa,
    [AI(RX_DO_NEG_CLASS)] =    do_neg_class_nfa,
    [AI(RX_DO_DOTALL)] =       do_dotall_nfa,
    [AI(RX_DO_CHAR)] =         do_char_nfa,
    [AI(RX_DO_RANGES)] =       do_range_nfa,
    [AI(RX_DO_RANGE)] =        do_range_nfa,
    [AI(RX_DO_STAR)] =         do_star_nfa,
    [AI(RX_DO_PLUS)] =         do_plus_nfa,
    [AI(RX_DO_OPTIONAL)] =     do_optional_nfa,
    [AI(RX_DO_REPEAT_EXACT)] = do_repeat_exact_nfa
};

struct regex_parse_interface const nfa_parse_iface = {
    .result = RESULTFN nfa_to_result,
    .has_error = HASERRFN nfa_has_error,
    .error = ERRFN nfa_error,
    .actions = ACTIONS nfa_actions
};

bool nfa_context(struct nfa_context *context, struct regex_pattern const *patterns) {
    struct hash_table *defpats = hash_table(0);
    if (!defpats)
        return set_oom_error(context);

    struct hash_table *tagged_nfas = hash_table(sizeof (struct nfa));
    if (!tagged_nfas) {
        free_hash_table(defpats);
        return set_oom_error(context);
    }

    struct nfa_state_pool *pool = state_pool();
    if (!pool) {
        free_hash_table(defpats);
        free_hash_table(tagged_nfas);
        return set_oom_error(context);
    }

    reset_context(context);

    context->defpats = defpats;
    context->tagged_nfas = tagged_nfas;
    context->state_pools = context->state_pool = pool;

    return nfa_add_patterns(patterns, context);
}

bool nfa_add_patterns(struct regex_pattern const *pat, struct nfa_context *context) {
    if (!pat) return true;

    bool success = true;

    while (!regex_null_pattern(pat) && success) {
        success = nfa_regex(pat->sym, pat->tag, pat->pattern, context);
        pat++;
    }

    return success;
}

bool nfa_regex(int sym, char *tag, char *pattern, struct nfa_context *context) {
    if (nfa_has_error(context)) return false;

    if (htcontains(pattern, context->defpats))
        return set_duplicate_pattern_error(pattern, context);

    htinsert(pattern, NULL, context->defpats);

    struct regex_parse_context pcontext = regex_parse_context(context, nfa_parse_iface);
    struct nfa lastmach = nfa_gmachine(context);

    if (parse_regex(pattern, &pcontext)) {
        struct nfa nextmach = nfa_gmachine(context);

        if (tag && !tag_machine(tag, context)) return false;

        if (sym != RX_TAG_ONLY) {
            debug_pattern(sym, pattern);
            point(nextmach, setst(accepting_state(sym), context));
            if (runnable(lastmach))
                link_machines(lastmach, nextmach, context);
        } else {
            smachine(lastmach, context);
        }

        debug_state_table(context->state_pools);
        debug_tagged_nfas(context->tagged_nfas);
    } else {
        context->has_error = pcontext.has_error;
        context->error = pcontext.error;
        return false;
    }

    return true;
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
            case RX_CLASS_STATE:
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

void free_nfa_context(struct nfa_context *context) {
    if (!context) return;
    free_hash_table(context->defpats);
    free_hash_table(context->tagged_nfas);
    free_pools(context);
    reset_context(context);
}

static struct nfa_state **eps_closure(
    int *foundsym,
    struct nfa_state **cend,
    bool *already_on,
    struct nfa_state *state
) {
    if (already_on[state->id]) return cend;

    *cend++ = state;
    already_on[state->id] = true;

    switch (state->type) {
        case RX_EPSILON_STATE:
            cend = eps_closure(foundsym, cend, already_on, state->next);
            break;
        case RX_BRANCH_STATE:
            cend = eps_closure(foundsym, cend, already_on, state->left);
            cend = eps_closure(foundsym, cend, already_on, state->right);
            break;
        case RX_ACCEPTING_STATE:
            if (foundsym && *foundsym == RX_REJECTED) *foundsym = state->sym;
            break;
        case RX_DOTALL_STATE:
        case RX_CLASS_STATE:
        case RX_CHAR_STATE:
            break;
    }

    return cend;
}

static bool matches_state(unsigned char c, struct nfa_state *state) {
    // state as a predicate function
    switch (state->type) {
        case RX_DOTALL_STATE:
            return true;
        case RX_CHAR_STATE:
            return state->ch == c;
        case RX_CLASS_STATE:
            return state->char_class[c];
        default:
            return false;
    }
}

static struct nfa_state **move(
    int *foundsym,
    struct nfa_state **nend,
    struct nfa_state **cstart,
    struct nfa_state **cend,
    bool *already_on,
    char c
) {
    struct nfa_state *state = NULL;

    while (cstart != cend && (state = *cstart)) {
        if (matches_state(c, state)) {
            nend = eps_closure(foundsym, nend, already_on, state->next);
        }

        cstart++;
    }

    return nend;
}

static void reset_match(struct nfa_match *match) {
    match->match_start = match->input = match->orig_input;
    match->match_loc = match->input_loc = regex_loc(1, 1);
    match->eof_seen = false;
}

static void reset_already_on(struct nfa_match *match) {
    memset(match->already_on, false, match->num_states);
}

bool nfa_start_match(char *input, struct nfa_match *match, struct nfa_context *context) {
    int num_states = context->num_states;
    bool *already_on = match->already_on;
    struct nfa_state **cstates = match->currstates,
                     **nstates = match->nextstates;

    if (already_on) reset_already_on(match);
    else            already_on = calloc(num_states, sizeof *already_on);

    if (!cstates) cstates = calloc(num_states, sizeof *cstates);
    if (!nstates) nstates = calloc(num_states, sizeof *nstates);

    if (already_on && cstates && nstates) {
        *match = (struct nfa_match) {
            .mach = nfa_gmachine(context),
            .num_states = num_states,
            .already_on = already_on,
            .currstates = cstates,
            .nextstates = nstates,
            .orig_input = input
        };

        reset_match(match);

        return true;
    }

    return false;
}

static int _nfa_match(struct nfa_match *match) {
    assert(match != NULL);
    struct nfa mach = match->mach;

    if (match->eof_seen) reset_match(match);

    char *input = match->input;
    struct regex_loc loc = match->input_loc;

    if (*input == '\0') {
        match->eof_seen = true;
        return RX_EOF;
    }

    if (!runnable(mach))
        return RX_REJECTED;

    match->match_start = input;
    match->match_loc = loc;

    bool *already_on = match->already_on;
    struct nfa_state **cstart, **cend, **nstart, **nend;
    cstart = cend = match->currstates;
    nstart = nend = match->nextstates;

    cend = eps_closure(NULL, cend, already_on, mach.start);

    debug("simulation, input: %s\n", input);
    debug_nfa_states(cstart, cend);

    // always consume at least one character
    char c = *input++;
    match->input = input;
    loc = match->input_loc = bump_regex_loc(c, loc);

    struct nfa_state **t = NULL;
    int retsym = RX_REJECTED;
    while ((cstart != cend) && c != '\0') {
        reset_already_on(match);

        // compute the set of next states from the current states
        int foundsym = RX_REJECTED;
        nend = move(&foundsym, nend, cstart, cend, already_on, c);

        // make the next states the current states
        cend = nend;
        t = cstart;
        cstart = nstart;
        nstart = t;
        nend = nstart;

        // commit our findings
        if (foundsym != RX_REJECTED) {
            retsym = foundsym;
            match->input = input;
            match->input_loc = loc;
        }

        // bump
        c = *input++;
        loc = bump_regex_loc(c, loc);

        debug("c = '%c', ", c);
        debug_nfa_states(cstart, cend);
    }

    reset_already_on(match);

    return retsym;
}

int nfa_match(struct nfa_match *match) {
    int sym;
    do sym = _nfa_match(match);
    while (sym == RX_SKIP);
    return sym;
}

struct regex_loc nfa_match_loc(struct nfa_match *match) {
    return match->match_loc;
}

void nfa_match_lexeme(char *lexeme, struct nfa_match *match) {
    size_t len = match->input - match->match_start;
    strncpy(lexeme, match->match_start, len);
    lexeme[len] = '\0';
}

void free_nfa_match(struct nfa_match *match) {
    if (match->currstates) free(match->currstates);
    if (match->nextstates) free(match->nextstates);
    if (match->already_on) free(match->already_on);

    *match = (struct nfa_match) {
        .mach = nullmach(),
        .input_loc = regex_loc(1, 1)
    };
}

