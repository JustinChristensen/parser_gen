#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <base/base.h>
#include <base/debug.h>
#include <base/hash_table.h>
#include <base/string.h>
#include <regex/base.h>
#include <regex/nfa.h>
#include "gram/check.h"
#include "gram/pack.h"
#include "gram/parser.h"
#include "gram/spec.h"

#include "internal/oom.c"

#define debug(...) debug_ns("gram_parser", __VA_ARGS__);

enum next_set {
    NULL_NS,
    // ... terminal symbols ...
    PATTERN_DEFS_NS = GM_ID_T + 1,
    GRAMMAR_NS,
    RULES_NS,
    ALTS_NS,
    RHSES_NS,
    RHS_NS
};

#define NEXT (enum gram_parser_symbol[])
static enum gram_parser_symbol *next_sets[] = {
    [NULL_NS]         = NULL,

    [GM_EOF_T]        = NEXT { GM_EOF_T, 0 },
    [GM_TAG_ONLY_T]   = NEXT { GM_TAG_ONLY_T, 0 },
    [GM_SKIP_T]       = NEXT { GM_SKIP_T, 0 },
    [GM_REGEX_T]      = NEXT { GM_REGEX_T, 0 },
    [GM_SECTION_T]    = NEXT { GM_SECTION_T, 0 },
    [GM_ASSIGN_T]     = NEXT { GM_ASSIGN_T, 0 },
    [GM_ALT_T]        = NEXT { GM_ALT_T, 0 },
    [GM_SEMICOLON_T]  = NEXT { GM_SEMICOLON_T, 0 },
    [GM_CHAR_T]       = NEXT { GM_CHAR_T, 0 },
    [GM_STRING_T]     = NEXT { GM_STRING_T, 0 },
    [GM_EMPTY_T]      = NEXT { GM_EMPTY_T, 0 },
    [GM_ID_T]         = NEXT { GM_ID_T, 0 },

    [PATTERN_DEFS_NS] = NEXT { GM_TAG_ONLY_T, GM_SKIP_T, GM_ID_T, GM_SECTION_T, GM_EOF_T,  0 },
    [GRAMMAR_NS]      = NEXT { GM_SECTION_T, GM_EOF_T,  0 },
    [RULES_NS]        = NEXT { GM_ID_T, GM_EOF_T, 0  },
    [ALTS_NS]         = NEXT { GM_ALT_T, GM_SEMICOLON_T, 0  },
    [RHSES_NS]        = NEXT { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, GM_ALT_T, GM_SEMICOLON_T, 0 },
    [RHS_NS]          = NEXT { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 }
};
#undef NEXT

static bool
next_set(enum next_set next, struct gram_parse_context *context) {
    context->next_set = next_sets[next];
    return true;
}

static char const *
str_for_sym(enum gram_parser_symbol sym) {
    switch (sym) {
        case GM_TAG_ONLY_T:            return "@";
        case GM_SKIP_T:                return "-";
        case GM_REGEX_T:               return "/abc/";
        case GM_SECTION_T:             return "---";
        case GM_ASSIGN_T:              return "=";
        case GM_ALT_T:                 return "|";
        case GM_SEMICOLON_T:           return ";";
        case GM_CHAR_T:                return "'a'";
        case GM_STRING_T:              return "\"abc\"";
        case GM_EMPTY_T:               return "$empty";
        case GM_ID_T:                  return "id";

        case GM_PARSER_SPEC_NT:        return "PARSER_SPEC";
        case GM_PATTERN_DEFS_NT:       return "PATTERN_DEFS";
        case GM_PATTERN_DEF_NT:        return "PATTERN_DEF";
        case GM_GRAMMAR_NT:            return "GRAMMAR";
        case GM_RULES_NT:              return "RULES";
        case GM_RULE_NT:               return "RULE";
        case GM_ALTS_NT:               return "ALTS";
        case GM_ALT_NT:                return "ALT";
        case GM_RHSES_NT:              return "RHSES";
        case GM_RHS_NT:                return "RHS";
        default:                       return "unrecognized token";
    }
}

static void
print_symbol_list(FILE *handle, enum gram_parser_symbol *sym) {
    if (*sym) {
        fprintf(handle, "%s", str_for_sym(*sym));
        sym++;
        while (*sym) fprintf(handle, ", %s", str_for_sym(*sym)), sym++;
    }
}

static char *permastr(char *str) {
    static char buf[BUFSIZ];
    strcpy(buf, str);
    return buf;
}

static bool
syntax_error(
    struct gram_parse_error *error,
    struct gram_parse_context *context
) {
    prod(error, ((struct gram_parse_error) {
        .type = GM_PARSER_SYNTAX_ERROR,
        .actual = gram_lookahead(context),
        .loc = gram_location(context),
        .expected = context->next_set
    }));

    debug("parse error:");
    if (debug_is("gram_parser"))
        print_symbol_list(stderr, context->next_set);
    debug("\n");

    return false;
}

static bool pattern_defined_error(
    struct gram_parse_error *error,
    char *tag, struct regex_loc loc, struct regex_loc prev_loc
) {
    *error = (struct gram_parse_error) {
        GM_PARSER_PATTERN_DEFINED_ERROR,
        .id = permastr(tag),
        .loc = loc,
        .prev_loc = prev_loc
    };

    return false;
}

static bool duplicate_pattern_error(
    struct gram_parse_error *error,
    char *pattern, struct regex_loc loc
) {
    *error = (struct gram_parse_error) {
        GM_PARSER_DUPLICATE_PATTERN_ERROR,
        .id = pattern,
        .loc = loc
    };

    return false;
}

static bool nonterm_defined_as_term_error(
    struct gram_parse_error *error,
    char *id, struct regex_loc loc, struct regex_loc term_loc
) {
    *error = (struct gram_parse_error) {
        GM_PARSER_NONTERM_DEFINED_AS_TERM_ERROR,
        .id = permastr(id),
        .loc = loc,
        .prev_loc = term_loc
    };

    return false;
}

static bool
scanner_error(struct gram_parse_error *error, struct regex_error scanerr) {
    prod(error, ((struct gram_parse_error) { .type = GM_PARSER_SCANNER_ERROR, .scanerr = scanerr }));
    return false;
}

static bool
peek(enum gram_parser_symbol expected, struct gram_parse_context *context) {
    return gram_lookahead(context) == expected;
}

static bool
expect(
    struct gram_parse_error *error,
    enum gram_parser_symbol expected,
    struct gram_parse_context *context
) {
    if (peek(expected, context)) {
        debug("success, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(context->sym));
        context->sym = gram_scan(context);
        context->next_set = NULL;
        return true;
    }

    debug("failure, expected \"%s\", actual \"%s\"\n",
        str_for_sym(expected), str_for_sym(context->sym));

    if (!context->next_set) next_set((enum next_set) expected, context);

    return syntax_error(error, context);
}

bool
gram_parse_context(struct gram_parse_error *error, struct gram_parse_context *context) {
    struct nfa_context scanner = { 0 };
    if (!nfa_context(&scanner, RX_PATTERNS {
        RX_ALPHA_(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
        RX_SPACE(RX_TAG_ONLY), RX_LINE_COMMENT(RX_SKIP),
        RX_REGEX(GM_REGEX_T),
        { GM_TAG_ONLY_T, NULL, "@" },
        { GM_SKIP_T, NULL, "-" },
        // FIXME: really, this should be handled by composing scanners,
        // i.e. root scanner: (patterns scanner, grammar scanner)
        // where the patterns scanner returns spaces and newline characters to the parser,
        // and the grammar scanner does not
        // { GM_REGEX_T, NULL, "{regex}\n" },
        { GM_SECTION_T, NULL, "---\n" },
        { GM_ASSIGN_T, NULL, "=" },
        { GM_ALT_T, NULL, "\\|" },
        { GM_SEMICOLON_T, NULL, ";" },
        { GM_EMPTY_T, NULL, GM_EMPTY_TOKEN },
        { GM_CHAR_T, NULL, "'(\\\\.|[^'])'" },
        { GM_STRING_T, NULL, "\"(\\\\.|[^\"])*\"" },
        { GM_ID_T, NULL, "{alpha_}{alnum_}*" },
        { RX_SKIP, NULL, "{space}+" },
        RX_END_PATTERNS
    })) {
        scanner_error(error, nfa_error(&scanner));
        free_nfa_context(&scanner);
        return false;
    }

    struct hash_table *symtab = hash_table(sizeof (struct gram_symbol_entry));
    if (!symtab) {
        free_nfa_context(&scanner);
        return oom_error(error, NULL);
    }

    *context = (struct gram_parse_context) { .symtab = symtab, .scanner = scanner };

    return true;
}

void
free_gram_parse_context(struct gram_parse_context *context) {
    free_hash_table(context->symtab);
    free_nfa_match(&context->match);
    free_nfa_context(&context->scanner);
    *context = (struct gram_parse_context) { 0 };
}

#define ERROR_FMT_END "\n|\n"
#define SYNTAX_ERROR_FMT_START "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
#define OOM_ERROR_FMT_START "| Out of Memory\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"
#define PATTERN_DEFINED_FMT_START "| Pattern Defined\n|\n| Pattern %s at "
#define PATTERN_DEFINED_FMT_PREV "\n| was previously defined at "
#define NONTERM_TERM_FMT_START "| Non-Terminal Error\n|\n| Non-terminal %s at "
#define NONTERM_TERM_FMT_PREV "\n| was previously defined as a terminal at "
#define SYMBOL_NOT_DEFINED_FMT_START "| Symbol Not Defined\n|\n| Symbol %s at "
#define SYMBOL_NOT_DEFINED_FMT_END " has not been defined\n|\n"
#define DUPLICATE_PATTERN_FMT_START "| Duplicate Pattern\n|\n| Pattern %s at "
#define DUPLICATE_PATTERN_FMT_END " is a duplicate\n|\n"

void
print_gram_parse_error(FILE *handle, struct gram_parse_error error) {
    switch (error.type) {
        case GM_PARSER_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, str_for_sym(error.actual));
            print_symbol_list(handle, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_PARSER_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
        case GM_PARSER_PATTERN_DEFINED_ERROR:
            fprintf(handle, PATTERN_DEFINED_FMT_START, error.id);
            print_regex_loc(handle, error.loc);
            fprintf(handle, PATTERN_DEFINED_FMT_PREV);
            print_regex_loc(handle, error.prev_loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_PARSER_DUPLICATE_PATTERN_ERROR:
            fprintf(handle, DUPLICATE_PATTERN_FMT_START, error.id);
            print_regex_loc(handle, error.loc);
            fprintf(handle, DUPLICATE_PATTERN_FMT_END);
            break;
        case GM_PARSER_NONTERM_DEFINED_AS_TERM_ERROR:
            fprintf(handle, NONTERM_TERM_FMT_START, error.id);
            print_regex_loc(handle, error.loc);
            fprintf(handle, NONTERM_TERM_FMT_PREV);
            print_regex_loc(handle, error.prev_loc);
            fprintf(handle, ERROR_FMT_END);
            break;
        case GM_PARSER_SYMBOL_NOT_DEFINED_ERROR:
            fprintf(handle, SYMBOL_NOT_DEFINED_FMT_START, error.id);
            print_regex_loc(handle, error.loc);
            fprintf(handle, SYMBOL_NOT_DEFINED_FMT_END);
            break;
        case GM_PARSER_SCANNER_ERROR:
            print_regex_error(stderr, error.scanerr);
            break;
    }
}

bool
gram_start_scanning(struct gram_parse_error *error, char *input, struct gram_parse_context *context) {
    struct nfa_match match = { 0 };

    prod(error, ((struct gram_parse_error) { 0 }));

    if (nfa_start_match(input, &match, &context->scanner)) {
        htclear(context->symtab);
        context->stats = (struct gram_stats) { 0 };

        context->match = match;
        context->sym = gram_scan(context);
        next_set(NULL_NS, context);
        return true;
    }

    return oom_error(error, NULL);
}

enum gram_parser_symbol
gram_scan(struct gram_parse_context *context) {
    return nfa_match(&context->match);
}

enum gram_parser_symbol
gram_lookahead(struct gram_parse_context *context) {
    return context->sym;
}

struct regex_loc
gram_location(struct gram_parse_context *context) {
    return nfa_match_loc(&context->match);
}

bool
gram_lexeme(char *lexeme, struct gram_parse_context *context) {
    nfa_match_lexeme(lexeme, &context->match);
    return true;
}

void
print_gram_tokens(FILE *handle, char *spec) {
    struct gram_parse_context context = { 0 };
    struct gram_parse_error error;

    if (gram_parse_context(&error, &context) && gram_start_scanning(&error, spec, &context)) {
        enum gram_parser_symbol sym = gram_lookahead(&context);
        char buf[BUFSIZ];

        fprintf(handle, "%3s\t%9s\t%s\n", "sym", "location", "token");

        do {
            struct regex_loc loc = gram_location(&context);
            gram_lexeme(buf, &context);
            fprintf(handle, "%3d\t%4d:%-4d\t%s\n", sym, loc.line, loc.col, buf);
        } while ((sym = gram_scan(&context)) != GM_EOF_T);

        free_gram_parse_context(&context);
    } else {
        print_gram_parse_error(stderr, error);
    }
}

static void
debug_symbol_entry(FILE *handle, void const *entry) {
    struct gram_symbol_entry const *e = entry;

    if (e->type == GM_PATTERN_ENTRY) {
        fprintf(handle, "PATTERN");
    } else if (e->type == GM_TAG_ENTRY) {
        fprintf(handle, "TAG");
    } else {
        char *type = e->s.type == GM_TERM ? "TERM" : "NONTERM";
        fprintf(handle, "%d, %s", e->s.num, type);
    }
}

static void
debug_symbols(struct gram_parse_context *context) {
    debug("symbol table:\n");
    if (debug_is("gram_parser"))
        print_hash_entries(stderr, debug_symbol_entry, context->symtab);
}

static struct gram_symbol_entry term(struct gram_parse_context *context) {
    context->stats.patterns++;

    return (struct gram_symbol_entry) {
        GM_SYMBOL_ENTRY,
        { GM_TERM, .num = context->stats.terms++ },
        .defined = true,
        .first_loc = gram_location(context)
    };
}

static struct gram_symbol_entry nonterm(struct gram_parse_context *context) {
    return (struct gram_symbol_entry) {
        GM_SYMBOL_ENTRY,
        { GM_NONTERM, .num = context->stats.nonterms++ },
        .first_loc = gram_location(context)
    };
}

static struct gram_symbol_entry defined_nonterm(struct gram_parse_context *context) {
    struct gram_symbol_entry entry = nonterm(context);
    entry.defined = true;
    return entry;
}

static struct gram_symbol_entry pattern(struct gram_parse_context *context) {
    return (struct gram_symbol_entry) {
        GM_PATTERN_ENTRY,
        .first_loc = gram_location(context)
    };
}

static struct gram_symbol_entry tag_only(struct gram_parse_context *context) {
    context->stats.patterns++;

    return (struct gram_symbol_entry) {
        GM_TAG_ENTRY,
        .first_loc = gram_location(context)
    };
}

static bool tag_exists_error(
    struct gram_parse_error *error, char *tag,
    struct gram_symbol_entry *entry, struct gram_parse_context *context
) {
    return pattern_defined_error(error, tag, gram_location(context), entry->first_loc);
}

static bool pattern_exists_error(
    struct gram_parse_error *error, char *tag,
    struct gram_symbol_entry *entry, struct gram_parse_context *context
) {
    return duplicate_pattern_error(error, tag, gram_location(context));
}

static bool nonterm_term_error(
    struct gram_parse_error *error, char *id,
    struct gram_symbol_entry *entry, struct gram_parse_context *context
) {
    if (entry->s.type == GM_NONTERM) {
        entry->defined = true;
        return true;
    }

    return nonterm_defined_as_term_error(error, id, gram_location(context), entry->first_loc);
}

static bool insert_symbol(
    struct gram_parse_error *error,
    char *str,
    struct gram_symbol_entry (*mkentry)(struct gram_parse_context *context),
    bool (*sym_exists)(
        struct gram_parse_error *error, char *str,
        struct gram_symbol_entry *entry, struct gram_parse_context *context
    ),
    struct gram_parse_context *context
) {
    assert(str != NULL);
    struct hash_table *symtab = context->symtab;
    struct gram_symbol_entry *entry = htlookup(str, symtab);
    if (entry)
        return sym_exists ? (*sym_exists)(error, str, entry, context) : true;

    struct gram_symbol_entry new_entry = (*mkentry)(context);
    htinsert(str, &new_entry, symtab);
    return true;
}

static bool insert_tag(
    struct gram_parse_error *error,
    bool not_term, char *tag, struct gram_parse_context *context
) {
    gram_lexeme(tag, context);

    if (not_term)
        return insert_symbol(error, tag, tag_only, tag_exists_error, context);

    debug("inserting pattern %s\n", tag);

    return insert_symbol(error, tag, term, tag_exists_error, context);
}

static bool insert_pattern(
    struct gram_parse_error *error,
    char *pat, struct gram_parse_context *context
) {
    gram_lexeme(pat, context);
    return insert_symbol(error, pat, pattern, pattern_exists_error, context);
}

static bool
sast(void **result, void *p) {
    if (!result) return true;
    if (!p) return false;
    *result = p;
    return true;
}

static bool
start_rule(struct gram_parse_error *error, char *id, struct gram_parse_context *context) {
    gram_lexeme(id, context);

    if (insert_symbol(error, id, defined_nonterm, nonterm_term_error, context))
        return true;

    return false;
}

static void
addalts(char *id, int nalts, struct gram_parse_context *context) {
    assert(id != NULL);
    struct gram_symbol_entry *entry = htlookup(id, context->symtab);
    entry->nderives += nalts;
}

#define tryinit(error, result, fn, ...) \
    (sast((void **) (result), fn(__VA_ARGS__)) || oom_error((error), NULL))
#define link(head, rest) (head)->next = (rest)
#define addn(head, rest) if ((rest)) (head)->n += (rest)->n

static bool
parse_pattern_defs(struct gram_parse_error *error, struct gram_pattern_def **pdefs, struct gram_parse_context *context);
static bool
parse_pattern_def(struct gram_parse_error *error, struct gram_pattern_def **pdef, struct gram_parse_context *context);
static bool
parse_grammar(struct gram_parse_error *error, struct gram_rule **rules, struct gram_parse_context *context);
static bool
parse_rules(struct gram_parse_error *error, struct gram_rule **rules, struct gram_parse_context *context);
static bool
parse_rule(struct gram_parse_error *error, struct gram_rule **rule, struct gram_parse_context *context);
static bool
parse_alts(struct gram_parse_error *error, struct gram_alt **alts, struct gram_parse_context *context);
static bool
parse_alt(struct gram_parse_error *error, struct gram_alt **alt, struct gram_parse_context *context);
static bool
parse_rhses(struct gram_parse_error *error, struct gram_rhs **rhses, struct gram_parse_context *context);
static bool
parse_rhs(struct gram_parse_error *error, struct gram_rhs **rhs, struct gram_parse_context *context);

bool
gram_parse(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    char *input, struct gram_parse_context *context
) {
    assert(error != NULL);
    assert(spec != NULL);

    if (!gram_start_scanning(error, input, context)) return false;

    prod(spec, ((struct gram_parser_spec) { 0 }));

    next_set(PATTERN_DEFS_NS, context);
    struct gram_pattern_def *pdefs = NULL;
    if (parse_pattern_defs(error, &pdefs, context)) {
        next_set(GRAMMAR_NS, context);
        struct gram_rule *rules = NULL;
        if (parse_grammar(error, &rules, context) && expect(error, GM_EOF_T, context)) {
            debug_symbols(context);
            prod(spec, gram_parsed_spec(pdefs, rules, context->stats));
            return gram_pack(error, spec, context);
        }

        free_gram_pattern_def(pdefs);
        free_gram_rule(rules);

        return false;
    }

    return false;
}

static bool
parse_pattern_defs(
    struct gram_parse_error *error, struct gram_pattern_def **pdefs,
    struct gram_parse_context *context
) {
    prod(pdefs, NULL);

    if ((peek(GM_SECTION_T, context) || peek(GM_EOF_T, context)) && next_set(NULL_NS, context))
        return true; // ε

    struct gram_pattern_def *head = NULL;
    if (parse_pattern_def(error, &head, context)) {
        next_set(PATTERN_DEFS_NS, context);
        struct gram_pattern_def *rest = NULL;
        if (parse_pattern_defs(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(pdefs, head);
            return true;
        }

        free_gram_pattern_def(head);

        return false;
    }

    return false;
}

static bool
parse_pattern_def(
    struct gram_parse_error *error, struct gram_pattern_def **pdef,
    struct gram_parse_context *context
) {
    prod(pdef, NULL);

    bool tag_only = false;
    bool skip = false;
    if (peek(GM_TAG_ONLY_T, context) && expect(error, GM_TAG_ONLY_T, context))
        tag_only = true;
    else if (peek(GM_SKIP_T, context) && expect(error, GM_SKIP_T, context))
        skip = true;

    char idbuf[BUFSIZ] = "";
    struct regex_loc loc = gram_location(context);

    if (peek(GM_ID_T, context) && !insert_tag(error, tag_only || skip, idbuf, context))
        return false;

    if (expect(error, GM_ID_T, context)) {
        char patbuf[BUFSIZ] = "";

        if (peek(GM_REGEX_T, context) && !insert_pattern(error, patbuf, context))
            return false;

        if (expect(error, GM_REGEX_T, context)) {
            return tryinit(error, pdef,
                init_gram_pattern_def, loc, idbuf, patbuf, tag_only, skip, NULL);
        }
    }

    return false;
}

static bool
parse_grammar(
    struct gram_parse_error *error, struct gram_rule **rules,
    struct gram_parse_context *context
) {
    prod(rules, NULL);

    if (peek(GM_EOF_T, context) && next_set(NULL_NS, context))
        return true; // ε

    if (expect(error, GM_SECTION_T, context) && next_set(RULES_NS, context) &&
        parse_rules(error, rules, context))
        return true;

    return false;
}

static bool
parse_rules(
    struct gram_parse_error *error, struct gram_rule **rules,
    struct gram_parse_context *context
) {
    prod(rules, NULL);

    if (peek(GM_EOF_T, context)) return true; // ε

    struct gram_rule *head = NULL;
    if (parse_rule(error, &head, context)) {
        struct gram_rule *rest = NULL;
        if (next_set(RULES_NS, context) && parse_rules(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(rules, head);
            return true;
        }

        free_gram_rule(head);

        return false;
    }

    return false;
}

static bool
parse_rule(
    struct gram_parse_error *error, struct gram_rule **rule,
    struct gram_parse_context *context
) {
    prod(rule, NULL);

    char idbuf[BUFSIZ] = "";
    struct regex_loc loc = gram_location(context);

    if (peek(GM_ID_T, context) && !start_rule(error, idbuf, context))
        return false;

    if (expect(error, GM_ID_T, context) && expect(error, GM_ASSIGN_T, context)) {
        struct gram_alt *alts = NULL;
        if (parse_alt(error, &alts, context)) {
            struct gram_alt *rest = NULL;
            if (next_set(ALTS_NS, context) && parse_alts(error, &rest, context)) {
                link(alts, rest);
                addn(alts, rest);
                addalts(idbuf, alts->n, context);

                if (expect(error, GM_SEMICOLON_T, context) &&
                    tryinit(error, rule, init_gram_rule, loc, idbuf, alts, NULL))
                    return true;
            }

            free_gram_alt(alts);
        }
    }

    return false;
}

static bool
parse_alts(
    struct gram_parse_error *error, struct gram_alt **alts,
    struct gram_parse_context *context
) {
    prod(alts, NULL);

    if (peek(GM_SEMICOLON_T, context) && next_set(NULL_NS, context))
        return true; // ε

    struct gram_alt *head = NULL;
    if (expect(error, GM_ALT_T, context) && parse_alt(error, &head, context)) {
        struct gram_alt *rest = NULL;
        if (next_set(ALTS_NS, context) && parse_alts(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(alts, head);
            return true;
        }

        free_gram_alt(head);
    }

    return false;
}

static bool
parse_alt(
    struct gram_parse_error *error, struct gram_alt **alt,
    struct gram_parse_context *context
) {
    prod(alt, NULL);

    struct regex_loc loc = gram_location(context);

    struct gram_rhs *rhses = NULL;
    if (next_set(RHS_NS, context) && parse_rhs(error, &rhses, context)) {
        struct gram_rhs *rest = NULL;
        if (next_set(RHSES_NS, context) && parse_rhses(error, &rest, context)) {
            link(rhses, rest);
            addn(rhses, rest);

            if (!gram_rhses_empty(rhses))
                context->stats.rules++;

            if (tryinit(error, alt, init_gram_alt, loc, rhses, NULL))
                return true;
        }

        free_gram_rhs(rhses);
    }

    return false;
}

static bool
parse_rhses(
    struct gram_parse_error *error, struct gram_rhs **rhses,
    struct gram_parse_context *context
) {
    prod(rhses, NULL);

    if ((peek(GM_ALT_T, context) || peek(GM_SEMICOLON_T, context)) && next_set(NULL_NS, context))
        return true; // ε

    struct gram_rhs *head = NULL;
    if (parse_rhs(error, &head, context)) {
        struct gram_rhs *rest = NULL;
        if (next_set(RHSES_NS, context) && parse_rhses(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(rhses, head);
            return true;
        }

        free_gram_rhs(head);
    }

    return false;
}

static bool
parse_rhs(
    struct gram_parse_error *error, struct gram_rhs **rhs,
    struct gram_parse_context *context
) {
    prod(rhs, NULL);

    char symbuf[BUFSIZ] = "";
    struct regex_loc loc = gram_location(context);

    gram_lexeme(symbuf, context);

    if (peek(GM_ID_T, context) && expect(error, GM_ID_T, context)) {
        insert_symbol(NULL, symbuf, nonterm, NULL, context);
        context->stats.rsymbols++;
        return tryinit(error, rhs, init_id_gram_rhs, loc, symbuf, NULL);
    } else if (peek(GM_CHAR_T, context) && expect(error, GM_CHAR_T, context)) {
        insert_symbol(NULL, symbuf, term, NULL, context);
        context->stats.rsymbols++;
        return tryinit(error, rhs, init_char_gram_rhs, loc, symbuf, NULL);
    } else if (peek(GM_STRING_T, context) && expect(error, GM_STRING_T, context)) {
        insert_symbol(NULL, symbuf, term, NULL, context);
        context->stats.rsymbols++;
        return tryinit(error, rhs, init_string_gram_rhs, loc, symbuf, NULL);
    }

    return expect(error, GM_EMPTY_T, context) && tryinit(error, rhs, init_empty_gram_rhs, loc, NULL);
}

