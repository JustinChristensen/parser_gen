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
#include "gram/spec.h"
#include "gram/parser.h"

#define debug(...) debug_ns("gram_parser", __VA_ARGS__);

#define FIRST (enum gram_parser_symbol[])
static enum gram_parser_symbol *first_sets[] = {
    [GM_EOF_T]                = FIRST { GM_EOF_T, 0 },
    [GM_TAG_ONLY_T]           = FIRST { GM_TAG_ONLY_T, 0 },
    [GM_SKIP_T]               = FIRST { GM_SKIP_T, 0 },
    [GM_REGEX_T]              = FIRST { GM_REGEX_T, 0 },
    [GM_SECTION_T]            = FIRST { GM_SECTION_T, 0 },
    [GM_ASSIGN_T]             = FIRST { GM_ASSIGN_T, 0 },
    [GM_ALT_T]                = FIRST { GM_ALT_T, 0 },
    [GM_SEMICOLON_T]          = FIRST { GM_SEMICOLON_T, 0 },
    [GM_CHAR_T]               = FIRST { GM_CHAR_T, 0 },
    [GM_STRING_T]             = FIRST { GM_STRING_T, 0 },
    [GM_EMPTY_T]              = FIRST { GM_EMPTY_T, 0 },
    [GM_ID_T]                 = FIRST { GM_ID_T, 0 },

    [GM_PARSER_SPEC_NT]       = FIRST { GM_TAG_ONLY_T, GM_SKIP_T, GM_ID_T, GM_SECTION_T, GM_EOF_T,  0 },
    [GM_PATTERN_DEFS_NT]      = FIRST { GM_TAG_ONLY_T, GM_SKIP_T, GM_ID_T, GM_SECTION_T, GM_EOF_T, 0 },
    [GM_PATTERN_DEF_NT]       = FIRST { GM_TAG_ONLY_T, GM_SKIP_T, GM_ID_T, 0 },
    [GM_GRAMMAR_NT]           = FIRST { GM_SECTION_T, GM_EOF_T, 0 },
    [GM_RULES_NT]             = FIRST { GM_ID_T, GM_EOF_T, 0  },
    [GM_RULE_NT]              = FIRST { GM_ID_T, 0 },
    [GM_ALTS_NT]              = FIRST { GM_ALT_T, GM_SEMICOLON_T, 0  },
    [GM_ALT_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 },
    [GM_RHSES_NT]             = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, GM_ALT_T, GM_SEMICOLON_T, 0 },
    [GM_RHS_NT]               = FIRST { GM_ID_T, GM_CHAR_T, GM_STRING_T, GM_EMPTY_T, 0 }
};
#undef FIRST

static enum gram_parser_symbol *
first_set(enum gram_parser_symbol sym) {
    return first_sets[sym];
}

static char const *
str_for_sym(enum gram_parser_symbol sym) {
    switch (sym) {
        case GM_ERROR:                 return "unrecognized token";

        case GM_EOF_T:                 return "eof";
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

static bool
syntax_error(
    struct gram_parse_error *error,
    struct enum gram_parser_symbol expected,
    struct gram_parse_context *context
) {
    debug("parse error %s\n", str_for_sym(expected));

    prod(error, (struct gram_parse_error) {
        .type = GM_PARSE_SYNTAX_ERROR,
        .actual = gram_lookahead(context),
        .loc = gram_location(context),
        .expected = first_set(expected)
    });

    return false;
}

static bool
scanner_error(struct gram_parse_error *error, struct regex_error error) {
    prod(error, (struct gram_parse_error) { .type = GM_PARSE_SCANNER_ERROR, .scanerr = error });
    return false;
}

static bool
_oom_error(struct gram_parse_error *error, char *file, int col, void *p, ...) {
    va_list args;
    va_start(args, p);
    vfree(p, args);
    va_end(args);

    prod(error, (struct gram_parse_error) { .type = GM_PARSE_OOM_ERROR, .file = file, .col = col });

    return false;
}

#define oom_error(error, ...) _oom_error((error), __FILE__, __LINE__, __VA_ARGS__, NULL)

static bool
peek(enum gram_parser_symbol expected, struct gram_parse_context *context) {
    enum gram_parser_symbol const *s = first_set(expected);

    while (*s) {
        if (gram_lookahead(context) == *s) return true;
        else s++;
    }

    return false;
}

static bool
expect(enum gram_parser_symbol expected, struct gram_parse_context *context) {
    if (peek(expected, context)) {
        debug("success, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(context->sym));
        context->sym = gram_scan(context);
        return true;
    }

    debug("failure, expected \"%s\", actual \"%s\"\n",
        str_for_sym(expected), str_for_sym(context->sym));

    return syntax_error(error, expected, context);
}

bool
gram_parse_context(struct gram_parse_error *error, struct gram_parse_context *context) {
    struct nfa_context scanner = { 0 };
    if (!nfa_context(&scanner, RX_PATTERNS {
        RX_ALPHA_(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
        RX_SPACE(RX_TAG_ONLY), RX_LINE_COMMENT(RX_SKIP),
        RX_REGEX(RX_TAG_ONLY),
        { GM_TAG_ONLY_T, NULL, "@" },
        { GM_SKIP_T, NULL, "-" },
        { GM_REGEX_T, NULL, "{regex}\n" },
        { GM_SECTION_T, NULL, "---\n" },
        { GM_ASSIGN_T, NULL, "=" },
        { GM_ALT_T, NULL, "\\|" },
        { GM_SEMICOLON_T, NULL, ";" },
        { GM_EMPTY_T, NULL, "$empty" },
        { GM_END_T, NULL, "$end" },
        { GM_CHAR_T, NULL, "'(\\\\.|[^'])'" },
        { GM_STRING_T, NULL, "\"(\\\\.|[^\"])*\"" },
        { GM_ID_T, NULL, "{alpha_}{alnum_}*" },
        { RX_SKIP, NULL, "{space}+" },
        { GM_ERROR, NULL, "." },
        RX_END_PATTERNS
    })) {
        scanner_error(error, nfa_error(&scanner));
        free_nfa_context(&scanner);
        return false;
    }

    struct hash_table *symtab = hash_table(sizeof (struct gram_symbol));
    if (!symtab) {
        free_nfa_context(&scanner);
        return oom_error(error);
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

#define SYNTAX_ERROR_FMT_START "| Syntax Error\n|\n| Got: %s\n| Expected: "
#define SYNTAX_ERROR_FMT_LOC "\n|\n| At: "
#define SYNTAX_ERROR_FMT_END "\n|\n"
#define OOM_ERROR_FMT_START "| Out of Memory Error\n|\n"
#define OOM_ERROR_FMT_FILE "| At: %s:%d\n|\n"

void
print_gram_parse_error(FILE *handle, struct gram_parse_error error) {
    switch (error.type) {
        case GM_PARSE_SYNTAX_ERROR:
            fprintf(handle, SYNTAX_ERROR_FMT_START, str_for_sym(error.actual));
            print_symbol_list(handle, error.expected);
            fprintf(handle, SYNTAX_ERROR_FMT_LOC);
            print_regex_loc(handle, error.loc);
            fprintf(handle, SYNTAX_ERROR_FMT_END);
            break;
        case GM_PARSE_OOM_ERROR:
            fprintf(handle, OOM_ERROR_FMT_START);
            if (debug_is("oom"))
                fprintf(handle, OOM_ERROR_FMT_FILE, error.file, error.col);
            break;
        case GM_PARSE_SCANNER_ERROR:
            print_regex_error(stderr, error.scanerr);
            break;
    }
}

bool
gram_start_scanning(struct gram_parse_error *error, char *input, struct gram_parse_context *context) {
    struct nfa_match match = { 0 };

    if (nfa_start_match(input, &match, &context->scanner)) {
        context->stats = (struct gram_stats) { 0 };
        htclear(context->symtab);

        context->current_rule = NULL;
        context->match = match;
        context->sym = gram_scan(context);
        return true;
    }

    return oom_error(error);
}

enum gram_parser_symbol
gram_scan(struct gram_parse_context *context) {
    enum gram_parser_symbol sym = gram_lookahead(context);
    sym = nfa_match(&context->match);
    return sym == RX_EOF ? GM_EOF_T : sym;
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

struct hash_table *
gram_symtab(struct gram_parse_context const *context) {
    return context->symtab;
}

void
print_gram_tokens(FILE *handle, char *spec) {
    struct gram_parse_context context = { 0 };

    if (gram_parse_context(&context) && gram_start_scanning(spec, &context)) {
        enum gram_parser_symbol sym = gram_lookahead(&context);
        char buf[BUFSIZ];

        fprintf(handle, "%3s\t%9s\t%s\n", "sym", "location", "token");

        do {
            struct regex_loc loc = gram_location(&context);
            gram_lexeme(buf, &context);
            fprintf(handle, "%3d\t%4d:%-4d\t%s\n", sym, loc.line, loc.col, buf);
        } while ((sym = gram_scan(&context)) != GM_EOF_T);

        free_gram_parse_context(&context);
    }
}

static void
debug_symbol_entry(FILE *handle, void const *entry) {
    struct gram_symbol const *e = entry;
    char *type = e->type == GM_TERM ? "TERM" : "NONTERM";
    fprintf(handle, "%d, %s", e->num, type);
}

static void
debug_symbols(struct gram_parse_context *context) {
    debug("symbol table:\n");
    if (debug_is("gram_parser"))
        print_hash_entries(stderr, debug_symbol_entry, context->symtab);
}

static struct gram_symbol
term_symbol(struct gram_parse_context *context) {
    context->stats.patterns++;

    return (struct gram_symbol) {
        .type = GM_TERM,
        .num = context->stats.terms++
    };
}

static struct gram_symbol
nonterm_symbol(struct gram_parse_context *context) {
    return (struct gram_symbol) {
        .type = GM_NONTERM,
        .num = context->stats.nonterms++
    };
}

static int
insert_symbol(
    char *str,
    struct gram_symbol (*mkentry)(struct gram_parse_context *context),
    struct gram_parse_context *context
) {
    struct hash_table *symtab = context->symtab;
    struct gram_symbol *entry = htlookup(str, symtab);

    if (entry) return entry->num;

    struct gram_symbol new_entry = (*mkentry)(context);

    htinsert(str, &new_entry, symtab);

    return new_entry.num;
}

static void
set_current_rule(char *id, struct gram_parse_context *context) {
    context->current_rule = htlookup(id, context->symtab);
}

static void
addalts(int nalts, struct gram_parse_context *context) {
    context->current_rule->nrules += nalts;
}

static bool
sast(void **result, void *p) {
    if (!result) return true;
    if (!p) return false;
    *result = p;
    return true;
}

#define tryinit(error, result, fn, ...) (sast((result), fn(__VA_ARGS__)) || oom_error((error)))
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

    prod(spec, NULL);

    // GM_TAG_ONLY_T, GM_SKIP_T, GM_ID_T, GM_SECTION_T, GM_EOF_T
    struct gram_pattern_def *pdefs = NULL;
    if (parse_pattern_defs(error, &pdefs, context)) {
        struct gram_rule *rules = NULL;
        // GM_SECTION_T, GM_EOF_T
        if (parse_grammar(error, &rules, context) && expect(GM_EOF_T, context)) {
            prod(spec, gram_parsed_spec(pdefs, rules, context->stats));
            debug_symbols(context);
            return true;
        }

        free_gram_pattern_def(pdefs);
        free_rules(rules);

        return false;
    }

    // FIXME: syntax error handling
    // return syntax_error(GM_PARSER_SPEC_NT, context);
    return false;
}

static bool
parse_pattern_defs(
    struct gram_parse_error *error, struct gram_pattern_def **pdefs,
    struct gram_parse_context *context
) {
    prod(pdefs, NULL);

    if (peek(GM_SECTION_T, context) || peek(GM_EOF_T, context))
        return true; // ε

    struct gram_pattern_def *head = NULL;
    if (parse_pattern_def(error, &head, context)) {
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

    // return syntax_error(GM_PATTERN_DEFS_NT, context);
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
    char idbuf[BUFSIZ] = "";

    if (peek(GM_TAG_ONLY_T, context))
        expect(GM_TAG_ONLY_T, context), tag_only = true;
    else if (peek(GM_SKIP_T, context))
        expect(GM_SKIP_T, context), skip = true;

    if (gram_lexeme(idbuf, context) && expect(GM_ID_T, context)) {
        char patbuf[BUFSIZ] = "";

        if (gram_lexeme(patbuf, context) && expect(GM_REGEX_T, context)) {
            if (!tag_only && !skip) insert_symbol(idbuf, term_symbol, context);
            else                    context->stats.patterns++;

            return
                tryinit(error, pdef,
                init_gram_pattern_def, idbuf, patbuf, tag_only, skip, NULL);
        }
    }

    // return syntax_error(GM_PATTERN_DEF_NT, context);
    return false;
}

static bool
parse_grammar(
    struct gram_parse_error *error, struct gram_rule **rules,
    struct gram_parse_context *context
) {
    prod(rule, NULL);

    if (peek(GM_EOF_T, context)) return true; // ε

    if (expect(GM_SECTION_T, context) && parse_rules(error, rules, context))
        return true;

    // return syntax_error(GM_GRAMMAR_NT, context);
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
        if (parse_rules(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(rules, head);
            return true;
        }

        free_gram_rule(head);

        return false;
    }

    // return syntax_error(GM_RULES_NT, context);
    return false;
}

static bool
parse_rule(
    struct gram_parse_error *error, struct gram_rule **rule,
    struct gram_parse_context *context
) {
    prod(rule, NULL);
    char idbuf[BUFSIZ] = "";

    if (gram_lexeme(idbuf, context) && expect(GM_ID_T, context)) {
        insert_symbol(idbuf, nonterm_symbol, context);
        set_current_rule(idbuf, context);

        struct gram_alt *alts = NULL;
        if (expect(GM_ASSIGN_T, context) && parse_alt(error, &alts, context)) {
            struct gram_alt *rest = NULL;
            if (parse_alts(error, &rest, context)) {
                link(alts, rest);
                addn(alts, rest);
                addalts(alts->n, context);

                if (expect(GM_SEMICOLON_T, context) && tryinit(error, rule, init_gram_rule, idbuf, alts, NULL))
                    return true;
            }

            free_gram_alt(alts);

            return false;
        }
    }

    // return syntax_error(GM_RULE_NT, context);
    return false;
}

static bool
parse_alts(
    struct gram_parse_error *error, struct gram_alt **alts,
    struct gram_parse_context *context
) {
    prod(alts, NULL);

    if (peek(GM_SEMICOLON_T, context)) return true; // ε

    struct gram_alt *head = NULL;
    if (expect(GM_ALT_T, context) && parse_alt(error, &head, context)) {
        struct gram_alt *rest = NULL;
        if (parse_alts(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(alts, head);
            return true;
        }

        free_gram_alt(head);

        return false;
    }

    // return syntax_error(GM_ALTS_NT, context);
    return false;
}

static bool
parse_alt(
    struct gram_parse_error *error, struct gram_alt **alt,
    struct gram_parse_context *context
) {
    prod(alt, NULL);

    struct gram_rhs *rhses = NULL;
    if (parse_rhs(error, &rhses, context)) {
        struct gram_rhs *rest = NULL;
        if (parse_rhses(error, &rest, context)) {
            link(rhses, rest);
            addn(rhses, rest);
            context->stats.rules++;

            if (tryinit(error, alt, init_gram_alt, rhses, NULL))
                return true;
        }

        free_gram_rhs(rhses);

        return false;
    }

    // return syntax_error(GM_ALT_NT, context);
    return false;
}

static bool
parse_rhses(
    struct gram_parse_error *error, struct gram_rhs **rhses,
    struct gram_parse_context *context
) {
    prod(rhses, NULL);

    if (peek(GM_ALT_T, context) || peek(GM_SEMICOLON_T, context))
        return true; // ε

    struct gram_rhs *head = NULL;
    if (parse_rhs(error, &head, context)) {
        struct gram_rhs *rest = NULL;
        if (parse_rhses(error, &rest, context)) {
            link(head, rest);
            addn(head, rest);
            prod(rhses, head);
            return true;
        }

        free_gram_rhs(head);

        return false;
    }

    // return syntax_error(GM_RHSES_NT, context);
    return false;
}

static bool
parse_rhs(
    struct gram_parse_error *error, struct gram_rhs **rhs,
    struct gram_parse_context *context
) {
    prod(rhs, NULL);

    char symbuf[BUFSIZ] = "";

    if (peek(GM_ID_T, context) &&
        gram_lexeme(symbuf, context) &&
        expect(GM_ID_T, context)) {
        insert_symbol(symbuf, nonterm_symbol, context);
        return tryinit(error, rhs, init_id_gram_rhs, symbuf, NULL);
    } else if (peek(GM_CHAR_T, context) &&
        gram_lexeme(symbuf, context) &&
        expect(GM_CHAR_T, context)) {
        insert_symbol(symbuf, term_symbol, context);
        return tryinit(error, rhs, init_char_gram_rhs, symbuf, NULL);
    } else if (peek(GM_STRING_T, context) &&
        gram_lexeme(symbuf, context) &&
        expect(GM_STRING_T, context)) {
        insert_symbol(symbuf, term_symbol, context);
        return tryinit(error, rhs, init_string_gram_rhs, symbuf, NULL);
    } else if (peek(GM_EMPTY_T, context) && expect(GM_EMPTY_T, context)) {
        return tryinit(error, rhs, init_empty_gram_rhs, NULL);
    }

    // return syntax_error(GM_RHS_NT, context);
    return false;
}

