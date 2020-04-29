#ifndef GRAM_GEN_C_
#define GRAM_GEN_C_ 1

#include <stdbool.h>
#include <regex/nfa.h>
#include <gram/ll.h>

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

static unsigned rulesize(gram_sym_no *s) {
    unsigned size = 0;
    while (*s++) size++;
    return size;
}

#endif // GRAM_GEN_C_
