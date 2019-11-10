#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "base/intset.h"
#include "regex/dfa.h"
#include "regex/parser.h"
#include "regex/result_types.h"

struct dfa_node symbol_node(unsigned int id, char symbol, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = SYMBOL_NODE,
        .id = id,
        .symbol = symbol,
        .pos = pos
    };
}

struct dfa_node empty_node(unsigned int id, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = EMPTY_NODE,
        .id = id,
        .pos = pos
    };
}

struct dfa_node dotall_node(unsigned int id, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = DOTALL_NODE,
        .id = id,
        .pos = pos
    };
}

struct dfa_node alt_node(unsigned int id, unsigned int left, unsigned int right, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = ALT_NODE,
        .id = id,
        .left = left,
        .right = right,
        .pos = pos
    };
}

struct dfa_node cat_node(unsigned int id, unsigned int left, unsigned int right, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = CAT_NODE,
        .id = id,
        .left = left,
        .right = right,
        .pos = pos
    };
}

struct dfa_node star_node(unsigned int id, unsigned int node, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = STAR_NODE,
        .id = id,
        .node = node,
        .pos = pos
    };
}

struct dfa_node plus_node(unsigned int id, unsigned int node, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = PLUS_NODE,
        .id = id,
        .node = node,
        .pos = pos
    };
}

struct dfa_node optional_node(unsigned int id, unsigned int node, struct dfa_pos pos) {
    return (struct dfa_node) {
        .type = OPTIONAL_NODE,
        .id = id,
        .node = node,
        .pos = pos
    };
}

struct dfa_context dfa_context(struct dfa_node *nodes) {
    return (struct dfa_context) {
        .nodes = nodes,
        .n = 0,
        .has_error = false
    };
}

void sdfanode(struct dfa_context *context, struct dfa_node node) {
    apush(&node, context->nodes);
    context->root = node;
}

struct dfa_node gdfanode(struct dfa_context *context) {
    return context->root;
}

union rval dfa_node_to_rval(struct dfa_context *context) {
    return (union rval) { .node = gdfanode(context) };
}

void free_dfa_context(struct dfa_context *context);


void noop_dfa(struct dfa_context *context, union rval _) {}

void do_empty_dfa(struct dfa_context *context, union rval _) {
    sdfanode(context, empty_node(context->n++, (struct dfa_pos) { .nullable = true }));
}

void do_alt_dfa(struct dfa_context *context, union rval lnode) {
    unsigned int id = context->n++;
    struct dfa_node left = lnode.node, right = gdfanode(context);
    struct dfa_pos lpos = left.pos, rpos = right.pos;

    sdfanode(context, alt_node(id, left.id, right.id, (struct dfa_pos) {
        .nullable = lpos.nullable || rpos.nullable,
        .firstpos = sunion(lpos.firstpos, rpos.firstpos)
        .lastpos = sunion(lpos.lastpos, rpos.lastpos)
    }));
}

void do_cat_dfa(struct nfa_context *context, union rval lnode) {
    unsigned int id = context->n++;
    struct dfa_node left = lnode.node, right = gdfanode(context);
    struct dfa_pos lpos = left.pos, rpos = right.pos;

    sdfanode(context, alt_node(id, left.id, right.id, (struct dfa_pos) {
        .nullable = lpos.nullable && rpos.nullable,
        .firstpos = lpos.nullable ? sunion(lpos.firstpos, rpos.firstpos) : lpos.firstpos,
        .lastpos = rpos.nullable ? sunion(lpos.lastpos, rpos.lastpos) : rpos.lastpos
    }));
}

void do_dotall_dfa(struct dfa_context *context, union rval _) {
    unsigned int id = context->n++;

    sdfanode(context, dotall_node(id, (struct dfa_pos) {
        .nullable = false,
        .firstpos = sinsert(id, NULL),
        .lastpos = sinsert(id, NULL)
    }));
}

void do_symbol_dfa(struct dfa_context *context, union rval sym) {
    unsigned int id = context->n++;

    sdfanode(context, symbol_node(id, sym.sym, (struct dfa_pos) {
        .nullable = false,
        .firstpos = sinsert(id, NULL),
        .lastpos = sinsert(id, NULL)
    }));
}

void do_star_dfa(struct dfa_context *context, union rval _) {
    unsigned int id = context->n++;
    struct dfa_node node = gdfanode(context)

    sdfanode(context, star_node(id, node.id, (struct dfa_pos) {
        .nullable = true,
        .firstpos = node.pos.firstpos,
        .lastpos = node.pos.lastpos
    }));
}

void do_plus_dfa(struct dfa_context *context, union rval _) {
    unsigned int id = context->n++;
    struct dfa_node node = gdfanode(context)

    sdfanode(context, plus_node(id, node.id, (struct dfa_pos) {
        .nullable = false,
        .firstpos = node.pos.firstpos,
        .lastpos = node.pos.lastpos
    }));
}

void do_optional_dfa(struct nfa_context *context, union rval _) {
    unsigned int id = context->n++;
    struct dfa_node node = gdfanode(context)

    sdfanode(context, optional_node(id, node.id, (struct dfa_pos) {
        .nullable = true,
        .firstpos = node.pos.firstpos,
        .lastpos = node.pos.lastpos
    }));
}


