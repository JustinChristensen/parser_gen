#ifndef GRAM_GEN_C_
#define GRAM_GEN_C_ 1

#include <stdbool.h>
#include <regex/nfa.h>

static struct regex_pattern const default_tagged_patterns[] = {
    RX_ALPHA(RX_TAG_ONLY), RX_ALPHA_(RX_TAG_ONLY),
    RX_ALNUM(RX_TAG_ONLY), RX_ALNUM_(RX_TAG_ONLY),
    RX_SPACE(RX_TAG_ONLY),
    RX_END_PATTERNS
};

static bool init_scanner(struct nfa_context *scanner, struct regex_pattern *patterns) {
    if (nfa_context(scanner, default_tagged_patterns) && nfa_add_patterns(patterns, scanner))
        return true;

    return false;
}

static char *sym_str(gram_sym_no s, char **symtab) {
    if (!s) return "unrecognized token";
    if (s == GM_EOF) return "eof";
    if (symtab && symtab[s]) return symtab[s];
    static char buf[13];
    sprintf(buf, "%u", s);
    return buf;
}

#endif // GRAM_GEN_C_
