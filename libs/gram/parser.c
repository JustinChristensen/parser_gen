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
    [GM_COLON_T]      = NEXT { GM_COLON_T, 0 },
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
next_set(enum next_set next, struct gram_spec_parser *parser) {
    parser->next_set = next_sets[next];
    return true;
}

static char const *
str_for_sym(enum gram_parser_symbol sym) {
    switch (sym) {
        case GM_TAG_ONLY_T:            return "@";
        case GM_SKIP_T:                return "-";
        case GM_REGEX_T:               return "/abc/";
        case GM_SECTION_T:             return "---";
        case GM_COLON_T:               return ":";
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
syntax_error(struct gram_parse_error *error, struct gram_spec_parser *parser) {
    prod(error, ((struct gram_parse_error) {
        .type = GM_PARSER_SYNTAX_ERROR,
        .actual = gram_lookahead(parser),
        .loc = gram_location(parser),
        .expected = parser->next_set
    }));

    debug("parse error:");
    if (debug_is("gram_parser"))
        print_symbol_list(stderr, parser->next_set);
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
peek(enum gram_parser_symbol expected, struct gram_spec_parser *parser) {
    return gram_lookahead(parser) == expected;
}

static bool
expect(
    struct gram_parse_error *error,
    enum gram_parser_symbol expected,
    struct gram_spec_parser *parser
) {
    if (peek(expected, parser)) {
        debug("success, expected \"%s\", actual \"%s\"\n",
            str_for_sym(expected), str_for_sym(parser->sym));
        parser->sym = gram_scan(parser);
        parser->next_set = NULL;
        return true;
    }

    debug("failure, expected \"%s\", actual \"%s\"\n",
        str_for_sym(expected), str_for_sym(parser->sym));

    if (!parser->next_set) next_set((enum next_set) expected, parser);

    return syntax_error(error, parser);
}

bool
gram_spec_parser(struct gram_parse_error *error, struct gram_spec_parser *parser) {
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
        { GM_COLON_T, NULL, ":" },
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

    *parser = (struct gram_spec_parser) { .symtab = symtab, .scanner = scanner };

    return true;
}

void
free_gram_spec_parser(struct gram_spec_parser *parser) {
    free_hash_table(parser->symtab);
    free_nfa_match(&parser->match);
    free_nfa_context(&parser->scanner);
    *parser = (struct gram_spec_parser) { 0 };
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
gram_start_scanning(struct gram_parse_error *error, char *input, struct gram_spec_parser *parser) {
    struct nfa_match match = { 0 };

    prod(error, ((struct gram_parse_error) { 0 }));

    if (nfa_start_match(input, &match, &parser->scanner)) {
        htclear(parser->symtab);
        parser->stats = (struct gram_stats) { 0 };

        parser->match = match;
        parser->sym = gram_scan(parser);
        next_set(NULL_NS, parser);
        return true;
    }

    return oom_error(error, NULL);
}

enum gram_parser_symbol
gram_scan(struct gram_spec_parser *parser) {
    return nfa_match(&parser->match);
}

enum gram_parser_symbol
gram_lookahead(struct gram_spec_parser *parser) {
    return parser->sym;
}

struct regex_loc
gram_location(struct gram_spec_parser *parser) {
    return nfa_match_loc(&parser->match);
}

bool
gram_lexeme(char *lexeme, struct gram_spec_parser *parser) {
    nfa_match_lexeme(lexeme, &parser->match);
    return true;
}

void
print_gram_tokens(FILE *handle, char *spec) {
    struct gram_spec_parser parser = { 0 };
    struct gram_parse_error error;

    if (gram_spec_parser(&error, &parser) && gram_start_scanning(&error, spec, &parser)) {
        enum gram_parser_symbol sym = gram_lookahead(&parser);
        char buf[BUFSIZ];

        fprintf(handle, "%3s\t%9s\t%s\n", "sym", "location", "token");

        do {
            struct regex_loc loc = gram_location(&parser);
            gram_lexeme(buf, &parser);
            fprintf(handle, "%3d\t%4d:%-4d\t%s\n", sym, loc.line, loc.col, buf);
        } while ((sym = gram_scan(&parser)) != GM_EOF_T);

        free_gram_spec_parser(&parser);
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
debug_symbols(struct gram_spec_parser *parser) {
    debug("symbol table:\n");
    if (debug_is("gram_parser"))
        print_hash_entries(stderr, debug_symbol_entry, parser->symtab);
}

static struct gram_symbol_entry term(struct gram_spec_parser *parser) {
    parser->stats.patterns++;

    return (struct gram_symbol_entry) {
        GM_SYMBOL_ENTRY,
        { GM_TERM, .num = parser->stats.terms++ },
        .defined = true,
        .first_loc = gram_location(parser)
    };
}

static struct gram_symbol_entry nonterm(struct gram_spec_parser *parser) {
    return (struct gram_symbol_entry) {
        GM_SYMBOL_ENTRY,
        { GM_NONTERM, .num = parser->stats.nonterms++ },
        .first_loc = gram_location(parser)
    };
}

static struct gram_symbol_entry defined_nonterm(struct gram_spec_parser *parser) {
    struct gram_symbol_entry entry = nonterm(parser);
    entry.defined = true;
    return entry;
}

static struct gram_symbol_entry pattern(struct gram_spec_parser *parser) {
    return (struct gram_symbol_entry) {
        GM_PATTERN_ENTRY,
        .first_loc = gram_location(parser)
    };
}

static struct gram_symbol_entry tag_only(struct gram_spec_parser *parser) {
    parser->stats.patterns++;

    return (struct gram_symbol_entry) {
        GM_TAG_ENTRY,
        .first_loc = gram_location(parser)
    };
}

static bool tag_exists_error(
    struct gram_parse_error *error, char *tag,
    struct gram_symbol_entry *entry, struct gram_spec_parser *parser
) {
    return pattern_defined_error(error, tag, gram_location(parser), entry->first_loc);
}

static bool pattern_exists_error(
    struct gram_parse_error *error, char *tag,
    struct gram_symbol_entry *entry, struct gram_spec_parser *parser
) {
    return duplicate_pattern_error(error, tag, gram_location(parser));
}

static bool nonterm_term_error(
    struct gram_parse_error *error, char *id,
    struct gram_symbol_entry *entry, struct gram_spec_parser *parser
) {
    if (entry->s.type == GM_NONTERM) {
        entry->defined = true;
        return true;
    }

    return nonterm_defined_as_term_error(error, id, gram_location(parser), entry->first_loc);
}

static bool insert_symbol(
    struct gram_parse_error *error,
    char *str,
    struct gram_symbol_entry (*mkentry)(struct gram_spec_parser *parser),
    bool (*sym_exists)(
        struct gram_parse_error *error, char *str,
        struct gram_symbol_entry *entry, struct gram_spec_parser *parser
    ),
    struct gram_spec_parser *parser
) {
    assert(str != NULL);
    struct hash_table *symtab = parser->symtab;
    struct gram_symbol_entry *entry = htlookup(str, symtab);
    if (entry)
        return sym_exists ? (*sym_exists)(error, str, entry, parser) : true;

    struct gram_symbol_entry new_entry = (*mkentry)(parser);
    htinsert(str, &new_entry, symtab);
    return true;
}

static bool insert_tag(
    struct gram_parse_error *error,
    bool not_term, char *tag, struct gram_spec_parser *parser
) {
    gram_lexeme(tag, parser);

    if (not_term)
        return insert_symbol(error, tag, tag_only, tag_exists_error, parser);

    debug("inserting pattern %s\n", tag);

    return insert_symbol(error, tag, term, tag_exists_error, parser);
}

static bool insert_pattern(
    struct gram_parse_error *error,
    char *pat, struct gram_spec_parser *parser
) {
    gram_lexeme(pat, parser);
    return insert_symbol(error, pat, pattern, pattern_exists_error, parser);
}

static bool
sast(void **result, void *p) {
    if (!result) return true;
    if (!p) return false;
    *result = p;
    return true;
}

static bool
start_rule(struct gram_parse_error *error, char *id, struct gram_spec_parser *parser) {
    gram_lexeme(id, parser);

    if (insert_symbol(error, id, defined_nonterm, nonterm_term_error, parser))
        return true;

    return false;
}

static void
addalts(char *id, int nalts, struct gram_spec_parser *parser) {
    assert(id != NULL);
    struct gram_symbol_entry *entry = htlookup(id, parser->symtab);
    entry->nderives += nalts;
}

#define tryinit(error, result, fn, ...) \
    (sast((void **) (result), fn(__VA_ARGS__)) || oom_error((error), NULL))
#define link(head, rest) (head)->next = (rest)
#define addn(head, rest) if ((rest)) (head)->n += (rest)->n

static bool
parse_pattern_defs(struct gram_parse_error *error, struct gram_pattern_def **pdefs, struct gram_spec_parser *parser);
static bool
parse_pattern_def(struct gram_parse_error *error, struct gram_pattern_def **pdef, struct gram_spec_parser *parser);
static bool
parse_grammar(struct gram_parse_error *error, struct gram_rule **rules, struct gram_spec_parser *parser);
static bool
parse_rules(struct gram_parse_error *error, struct gram_rule **rules, struct gram_spec_parser *parser);
static bool
parse_rule(struct gram_parse_error *error, struct gram_rule **rule, struct gram_spec_parser *parser);
static bool
parse_alts(struct gram_parse_error *error, struct gram_alt **alts, struct gram_spec_parser *parser);
static bool
parse_alt(struct gram_parse_error *error, struct gram_alt **alt, struct gram_spec_parser *parser);
static bool
parse_rhses(struct gram_parse_error *error, struct gram_rhs **rhses, struct gram_spec_parser *parser);
static bool
parse_rhs(struct gram_parse_error *error, struct gram_rhs **rhs, struct gram_spec_parser *parser);

bool
gram_parse(
    struct gram_parse_error *error, struct gram_parser_spec *spec,
    char *input, struct gram_spec_parser *parser
) {
    assert(error != NULL);
    assert(spec != NULL);

    if (!gram_start_scanning(error, input, parser)) return false;

    prod(spec, ((struct gram_parser_spec) { 0 }));

    next_set(PATTERN_DEFS_NS, parser);
    struct gram_pattern_def *pdefs = NULL;
    if (parse_pattern_defs(error, &pdefs, parser)) {
        next_set(GRAMMAR_NS, parser);
        struct gram_rule *rules = NULL;
        if (parse_grammar(error, &rules, parser) && expect(error, GM_EOF_T, parser)) {
            debug_symbols(parser);
            prod(spec, gram_parsed_spec(pdefs, rules, parser->stats));
            return gram_pack(error, spec, parser);
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
    struct gram_spec_parser *parser
) {
    prod(pdefs, NULL);

    if ((peek(GM_SECTION_T, parser) || peek(GM_EOF_T, parser)) && next_set(NULL_NS, parser))
        return true; // ε

    struct gram_pattern_def *head = NULL;
    if (parse_pattern_def(error, &head, parser)) {
        next_set(PATTERN_DEFS_NS, parser);
        struct gram_pattern_def *rest = NULL;
        if (parse_pattern_defs(error, &rest, parser)) {
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
    struct gram_spec_parser *parser
) {
    prod(pdef, NULL);

    bool tag_only = false;
    bool skip = false;
    if (peek(GM_TAG_ONLY_T, parser) && expect(error, GM_TAG_ONLY_T, parser))
        tag_only = true;
    else if (peek(GM_SKIP_T, parser) && expect(error, GM_SKIP_T, parser))
        skip = true;

    char idbuf[BUFSIZ] = "";
    struct regex_loc loc = gram_location(parser);

    if (peek(GM_ID_T, parser) && !insert_tag(error, tag_only || skip, idbuf, parser))
        return false;

    if (expect(error, GM_ID_T, parser)) {
        char patbuf[BUFSIZ] = "";

        if (peek(GM_REGEX_T, parser) && !insert_pattern(error, patbuf, parser))
            return false;

        if (expect(error, GM_REGEX_T, parser)) {
            return tryinit(error, pdef,
                init_gram_pattern_def, loc, idbuf, patbuf, tag_only, skip, NULL);
        }
    }

    return false;
}

static bool
parse_grammar(
    struct gram_parse_error *error, struct gram_rule **rules,
    struct gram_spec_parser *parser
) {
    prod(rules, NULL);

    if (peek(GM_EOF_T, parser) && next_set(NULL_NS, parser))
        return true; // ε

    if (expect(error, GM_SECTION_T, parser) && next_set(RULES_NS, parser) &&
        parse_rules(error, rules, parser))
        return true;

    return false;
}

static bool
parse_rules(
    struct gram_parse_error *error, struct gram_rule **rules,
    struct gram_spec_parser *parser
) {
    prod(rules, NULL);

    if (peek(GM_EOF_T, parser)) return true; // ε

    struct gram_rule *head = NULL;
    if (parse_rule(error, &head, parser)) {
        struct gram_rule *rest = NULL;
        if (next_set(RULES_NS, parser) && parse_rules(error, &rest, parser)) {
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
    struct gram_spec_parser *parser
) {
    prod(rule, NULL);

    char idbuf[BUFSIZ] = "";
    struct regex_loc loc = gram_location(parser);

    if (peek(GM_ID_T, parser) && !start_rule(error, idbuf, parser))
        return false;

    if (expect(error, GM_ID_T, parser) && expect(error, GM_COLON_T, parser)) {
        struct gram_alt *alts = NULL;
        if (parse_alt(error, &alts, parser)) {
            struct gram_alt *rest = NULL;
            if (next_set(ALTS_NS, parser) && parse_alts(error, &rest, parser)) {
                link(alts, rest);
                addn(alts, rest);
                addalts(idbuf, alts->n, parser);

                if (expect(error, GM_SEMICOLON_T, parser) &&
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
    struct gram_spec_parser *parser
) {
    prod(alts, NULL);

    if (peek(GM_SEMICOLON_T, parser) && next_set(NULL_NS, parser))
        return true; // ε

    struct gram_alt *head = NULL;
    if (expect(error, GM_ALT_T, parser) && parse_alt(error, &head, parser)) {
        struct gram_alt *rest = NULL;
        if (next_set(ALTS_NS, parser) && parse_alts(error, &rest, parser)) {
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
    struct gram_spec_parser *parser
) {
    prod(alt, NULL);

    struct regex_loc loc = gram_location(parser);

    struct gram_rhs *rhses = NULL;
    if (next_set(RHS_NS, parser) && parse_rhs(error, &rhses, parser)) {
        struct gram_rhs *rest = NULL;
        if (next_set(RHSES_NS, parser) && parse_rhses(error, &rest, parser)) {
            link(rhses, rest);
            addn(rhses, rest);

            if (!gram_rhses_empty(rhses))
                parser->stats.rules++;

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
    struct gram_spec_parser *parser
) {
    prod(rhses, NULL);

    if ((peek(GM_ALT_T, parser) || peek(GM_SEMICOLON_T, parser)) && next_set(NULL_NS, parser))
        return true; // ε

    struct gram_rhs *head = NULL;
    if (parse_rhs(error, &head, parser)) {
        struct gram_rhs *rest = NULL;
        if (next_set(RHSES_NS, parser) && parse_rhses(error, &rest, parser)) {
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
    struct gram_spec_parser *parser
) {
    prod(rhs, NULL);

    char symbuf[BUFSIZ] = "";
    struct regex_loc loc = gram_location(parser);

    gram_lexeme(symbuf, parser);

    if (peek(GM_ID_T, parser) && expect(error, GM_ID_T, parser)) {
        insert_symbol(NULL, symbuf, nonterm, NULL, parser);
        parser->stats.rsymbols++;
        return tryinit(error, rhs, init_id_gram_rhs, loc, symbuf, NULL);
    } else if (peek(GM_CHAR_T, parser) && expect(error, GM_CHAR_T, parser)) {
        insert_symbol(NULL, symbuf, term, NULL, parser);
        parser->stats.rsymbols++;
        return tryinit(error, rhs, init_char_gram_rhs, loc, symbuf, NULL);
    } else if (peek(GM_STRING_T, parser) && expect(error, GM_STRING_T, parser)) {
        insert_symbol(NULL, symbuf, term, NULL, parser);
        parser->stats.rsymbols++;
        return tryinit(error, rhs, init_string_gram_rhs, loc, symbuf, NULL);
    }

    return expect(error, GM_EMPTY_T, parser) && tryinit(error, rhs, init_empty_gram_rhs, loc, NULL);
}

