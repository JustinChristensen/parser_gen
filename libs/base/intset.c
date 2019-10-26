#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include "base/intset.h"
#include "base/ord.h"
#include "base/string.h"
#include "base/array.h"
#include "base/bits.h"

static void print_intset_node(struct intset const *set) {
#if PRINT_BINARY
    printf("%p (", set);
    printbits(set->pfix);
    printf(", ");
    printbits(set->mask);
    printf(", %p, %p)", set->left, set->right);
#else
    printf("%p (%"PRIu64", %"PRIu64", %p, %p)",
        set, set->pfix, set->mask, set->left, set->right);
#endif
}

static bool is_branch(struct intset const *set) {
    return set->left != NULL;
}

static bool prefix_upto_branch_matches(uint64_t kfix, struct intset const *set) {
    return prefix_upto_branch(kfix, set->mask) == set->pfix;
}

static bool bmchecki(uint64_t *out, uint64_t pfix, uint64_t bitmap, int i) {
    // check the ith bit
    uint64_t x = bitmap & (BIT << i);

    // we found a set bit, notify the caller
    if (x) {
        *out = pfix | ctz(x);
        return true;
    }

    return false;
}

struct intset intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    return (struct intset) { pfix, mask, left, right };
}

static struct intset *make_branch(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    struct intset *set = NULL;

    if (!right) {
        set = left;
    } else if (!left) {
        set = right;
    } else {
        set = init_intset(pfix, mask, left, right);
    }

    return set;
}

struct intset *make_leaf(uint64_t pfix, uint64_t mask) {
    struct intset *set = NULL;

    if (mask) {
        set = init_intset(pfix, mask, NULL, NULL);
    }

    return set;
}

struct intset *init_intset(uint64_t pfix, uint64_t mask, struct intset *left, struct intset *right) {
    struct intset *set = malloc(sizeof *set);
    assert(set != NULL);
    *set = intset(pfix, mask, left, right);
    return set;
}

bool siterator(struct intset const *set, struct intset_iterator *it) {
    if (!it || !set) return false;
    it->stack = init_array(sizeof set, IT_STACK_SIZE, 0, 0);
    reset_siterator(it);
    it->root = set;
    return true;
}

// iterate all nodes
bool snextnode(struct intset const **out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct array *stack = it->stack;

    // if we're at the root, push the root node onto the stack
    if (it->at_root) apush(&it->root, it->stack);

    if (aempty(stack)) {
        *out = NULL;
        reset_siterator(it);
        return false;
    } else {
        struct intset *set = NULL;

        apop(&set, stack);

        if (is_branch(set)) {
            // if we're not at the root or the prefix is not negative
            // go right first (larger numbers)
            if (it->at_root && set->mask > INT64_MAX) {
                apush(&set->left, stack);
                apush(&set->right, stack);
            } else {
                apush(&set->right, stack);
                apush(&set->left, stack);
            }
        }

        // used by the bitmap and int iterators
        // DO NOT REMOVE
        it->set = set;
        it->at_root = false;

        // output
        *out = set;

        return true;
    }
}

// iterate leaf nodes
bool snextleaf(struct intset const **out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct intset const *set = NULL;
    bool res = true;

    while ((res = snextnode(&set, it))) {
        // skip branches
        if (is_branch(set)) continue;
        break;
    }

    // output
    *out = set;

    return res;
}

bool snextbitmap(int *out, struct intset_iterator *it) {
    if (!out || !it || !it->set) return false;

    struct intset const *set = it->set;

    uint64_t x;
    for (int i = it->i; i < WORDBITS; (it->i = ++i)) {
        if (bmchecki(&x, set->pfix, set->mask, i)) {
            it->i++;
            *out = (int) x;
            return true;
        }
    }

    it->i = 0;

    return false;
}

bool snext(int *out, struct intset_iterator *it) {
    if (!out || !it) return false;
    struct intset const *set = it->set;

    while (true) {
        if (it->i == 0 && !snextleaf(&set, it))
            break;

        if (snextbitmap(out, it))
            return true;
    }

    return false;
}

void reset_siterator(struct intset_iterator *it) {
    it->at_root = true;
    it->set = NULL;
    it->i = 0;
    areset(it->stack);
}

bool selem(int k, struct intset const *set) {
    struct array *stack = init_array(sizeof set, IT_STACK_SIZE, 0, 0);
    bool elem = false;
    uint64_t kfix = prefix(k),
            bmap = bitmap(k);

    apush(&set, stack);

    while (!aempty(stack)) {
        apop(&set, stack);

        if (is_branch(set)) {
            if (prefix_upto_branch_matches(kfix, set)) {
                if (zero(kfix, set->mask)) {
                    apush((void **) &set->left, stack);
                } else {
                    apush((void **) &set->right, stack);
                }

                continue;
            }
        } else if (set->pfix == kfix && (set->mask & bmap) != 0) {
            elem = true;
        }

        break;
    }

    free_array(stack);

    return elem;
}

static struct intset *link(struct intset const *newleaf, struct intset const *set) {
    struct intset const *right = newleaf;
    uint64_t brm = branch_mask(newleaf->pfix, set->pfix);

    if (zero(newleaf->pfix, brm)) {
        right = set;
        set = newleaf;
    }

    // the new branch node will have the leading bits of the key
    // up to the where the key and the keys in the branch diverge
    return init_intset(prefix_upto_branch(newleaf->pfix, brm), brm, (struct intset *) set, (struct intset *) right);
}

static struct intset *_sinsert(uint64_t kfix, uint64_t bitmap, struct intset *set) {
    if (!set) { // nil
        set = init_intset(kfix, bitmap, NULL, NULL);
    } else if (is_branch(set)) {
        // do the key's prefix and branch prefix match?
        if (!prefix_upto_branch_matches(kfix, set)) {
            set = link(init_intset(kfix, bitmap, NULL, NULL), set);
        } else if (zero(kfix, set->mask)) { // branch does not match, go left
            set->left = _sinsert(kfix, bitmap, set->left);
        } else { // branch matches, go right
            set->right = _sinsert(kfix, bitmap, set->right);
        }
    } else  { // leaf node
        if (set->pfix == kfix) {
            set->mask |= bitmap;
        } else {
            set = link(init_intset(kfix, bitmap, NULL, NULL), set);
        }
    }

    return set;
}

struct intset *sinsert(int k, struct intset *set) {
    return _sinsert(prefix(k), bitmap(k), set);
}

struct intset *slistinsert(int *k, size_t n, struct intset *set) {
    for (int i = 0; i < n; i++) {
        set = sinsert(k[i], set);
    }

    return set;
}

struct intset *sclone(struct intset const *set) {
    if (!set) return NULL;
    return init_intset(set->pfix, set->mask, sclone(set->left), sclone(set->right));
}

static struct intset *_sdelete(uint64_t kfix, uint64_t bitmap, struct intset *set) {
    if (!set) return NULL;

    if (is_branch(set)) {
        if (prefix_upto_branch_matches(kfix, set)) {
            struct intset *temp = NULL;
            if (zero(kfix, set->mask)) {
                set->left = _sdelete(kfix, bitmap, set->left);

                if (!set->left) {
                    temp = set->right;
                    free(set);
                    set = temp;
                }
            } else {
                set->right = _sdelete(kfix, bitmap, set->right);

                if (!set->right) {
                    temp = set->left;
                    free(set);
                    set = temp;
                }
            }
        }
    } else if (set->pfix == kfix) {
        set->mask = set->mask & ~bitmap;

        if (!set->mask) {
            free_intset(set);
            set = NULL;
        }
    }

    return set;
}

struct intset *sdelete(int k, struct intset *set) {
    return _sdelete(prefix(k), bitmap(k), set);
}

bool intseteq(struct intset const *s, struct intset const *t) {
    if (s == NULL && t == NULL) return true;

    bool eq = s && t;

    if (eq) {
        struct array *stack = init_array(sizeof s, IT_STACK_SIZE, 0, 0);

        apush(&s, stack);
        apush(&t, stack);

        while (eq && !aempty(stack)) {
            apop(&s, stack);
            apop(&t, stack);

            // branch nodes must always have two children
            if (s == NULL && t == NULL) continue;

            if (s->pfix == t->pfix && s->mask == t->mask) {
                apush((void **) &s->right, stack);
                apush((void **) &t->right, stack);
                apush((void **) &s->left, stack);
                apush((void **) &t->left, stack);
            } else {
                eq = false;
            }
        }

        free_array(stack);
    }

    return eq;
}

static struct intset *unify_branches(struct intset const *s, struct intset const *t) {
    struct intset *u = NULL;

    if (!prefix_upto_branch_matches(s->pfix, t)) {
        u = link(sclone(s), sclone(t));
    } else if (zero(s->pfix, t->mask)) {
        u = init_intset(t->pfix, t->mask, sunion(s, t->left), sclone(t->right));
    } else {
        u = init_intset(t->pfix, t->mask, sclone(t->left), sunion(s, t->right));
    }

    return u;
}

struct intset *sunion(struct intset const *s, struct intset const *t) {
    if (s == NULL && t == NULL) return NULL;
    if (!s) return sclone(t);
    if (!t) return sclone(s);

    struct intset *u = NULL;

    if (is_branch(s) && is_branch(t)) { // both nodes are branches
        if (s->mask < t->mask) { // t has the greater branch mask
            u = unify_branches(s, t);
        } else if (t->mask < s->mask) { // s has the greater branch mask
            u = unify_branches(t, s);
        } else if (t->pfix == s->pfix) {
            u = init_intset(s->pfix, s->mask,
                    sunion(s->left, t->left),
                    sunion(s->right, t->right));
        } else {
            u = link(sclone(s), sclone(t));
        }
    } else if (is_branch(s)) { // s is a branch, t is a leaf
        u = _sinsert(t->pfix, t->mask, sclone(s));
    } else { // t is a branch or leaf, s is a leaf
        u = _sinsert(s->pfix, s->mask, sclone(t));
    }

    return u;
}

static struct intset *intersect_branches(struct intset const *s, struct intset const *t) {
    struct intset *u = NULL;

    if (prefix_upto_branch_matches(s->pfix, t)) {
        // t is the greater node, so we need to determine
        // which of t's children to try next
        if (zero(s->pfix, t->mask)) {
            u = sintersection(s, t->left);
        } else {
            u = sintersection(s, t->right);
        }
    }

    return u;
}

struct intset *sintersection(struct intset const *s, struct intset const *t) {
    if (s == NULL || t == NULL) return NULL;

    struct intset *u = NULL;

    if (is_branch(s) && is_branch(t)) { // both nodes are branches
        if (s->mask < t->mask) { // t has the greater branch mask
            u = intersect_branches(s, t);
        } else if (t->mask < s->mask) { // s has the greater branch mask
            u = intersect_branches(t, s);
        } else if (t->pfix == s->pfix) {
            // s and t align, create a new branch from the intersection
            // of their respective children
            u = make_branch(s->pfix, s->mask,
                    sintersection(s->left, t->left),
                    sintersection(s->right, t->right));
        }
    } else if (is_branch(s) && prefix_upto_branch_matches(t->pfix, s)) { // s is a branch, t is a leaf
        // lets see if we can find the matching leaf in s
        if (zero(t->pfix, s->mask)) {
            u = sintersection(s->left, t);
        } else {
            u = sintersection(s->right, t);
        }
    } else if (is_branch(t) && prefix_upto_branch_matches(s->pfix, t)) { // t is a branch, s is a leaf
        // lets see if we can find the matching leaf in t
        if (zero(t->pfix, s->mask)) {
            u = sintersection(s, t->left);
        } else {
            u = sintersection(s, t->right);
        }
    } else if (s->pfix == t->pfix) { // both leaf nodes
        u = make_leaf(s->pfix, s->mask & t->mask);
    }

    return u;
}

static struct intset *differ_branches(struct intset const *s, struct intset const *t) {
    struct intset *u = NULL;

    if (prefix_upto_branch_matches(s->pfix, t)) {
        // t is the greater node, so we need to determine
        // which of t's children to try next
        if (zero(s->pfix, t->mask)) {
            u = sdifference(s, t->left);
        } else {
            u = sdifference(s, t->right);
        }
    } else {
        u = sclone(s);
    }

    return u;
}

struct intset *sdifference(struct intset const *s, struct intset const *t) {
    if (s == NULL) return NULL;
    if (t == NULL) return sclone(s);

    struct intset *u = NULL;

    if (is_branch(s) && is_branch(t)) {
        if (s->mask < t->mask) {
            u = differ_branches(s, t);
        } else if (t->mask < s->mask) {
            if (!prefix_upto_branch_matches(s->pfix, t)) return sclone(s);

            if (zero(s->pfix, t->mask)) {
                u = make_branch(s->pfix, s->mask, sdifference(s->left, t), sclone(s->right));
            } else {
                u = make_branch(s->pfix, s->mask, sclone(s->left), sdifference(s->right, t));
            }
        } else if (t->pfix == s->pfix) {
            u = make_branch(s->pfix, s->mask,
                    sdifference(s->left, t->left),
                    sdifference(s->right, t->right));
        }
    } else if (is_branch(s)) {
        u = _sdelete(t->pfix, t->mask, sclone(s));
    } else if (is_branch(t)) {
        if (prefix_upto_branch_matches(s->pfix, t)) {
            if (zero(s->pfix, t->mask)) {
                u = sdifference(s, t->left);
            } else {
                u = sdifference(s, t->right);
            }
        }
    } else if (s->pfix == t->pfix) {
        u = make_leaf(s->pfix, s->mask & ~t->mask);
    }

    return u;
}

bool sdisjoint(struct intset *s, struct intset const *t) {
    struct intset *u = sintersection(s, t);
    bool is_disj = snull(u);
    free_intset(u);
    return is_disj;
}

struct intset *sfromlist(int *k, size_t n) {
    return slistinsert(k, n, NULL);
}

int *stolist(struct intset const *set) {
    int *list = NULL;
    struct intset_iterator it;

    if (siterator(set, &it)) {
        list = calloc(ssize(set), sizeof *list);
        assert(list != NULL);
        int *i = list;
        while (snext(i++, &it));

        free_siterator(&it);
    }


    return list;
}

bool snull(struct intset const *set) {
    return ssize(set) == 0;
}

size_t ssize(struct intset const *set) {
    if (!set) return 0;

    size_t n = 0;

    struct intset_iterator it;
    if (siterator(set, &it)) {
        int x;
        while (snext(&x, &it)) n++;
        free_siterator(&it);
    }

    return n;
}

size_t streesize(struct intset const *set) {
    if (!set) return 0;

    size_t n = 0;

    struct intset_iterator it;
    if (siterator(set, &it)) {
        while (snextnode(&set, &it)) n++;
        free_siterator(&it);
    }

    return n;
}

static size_t _streedepth(struct intset const *set, int d) {
    if (!set) return d;
    d++;
    return max(_streedepth(set->left, d), _streedepth(set->right, d));
}

size_t streedepth(struct intset const *set) {
    return _streedepth(set, 0);
}

void print_intset(struct intset const *set) {
    if (!set) return;

    struct intset_iterator it;
    if (siterator(set, &it)) {
        int x;
        while (snext(&x, &it)) {
            printf("%d ", x);
        }

        free_siterator(&it);
    }
}

static void _print_intset_tree(struct intset const *set, int depth) {
    if (!set) return;
    indent(depth);
    print_intset_node(set);
    printf("\n");
    depth++;
    _print_intset_tree(set->left, depth);
    _print_intset_tree(set->right, depth);
}

void print_intset_tree(struct intset const *set) {
    _print_intset_tree(set, 0);
}

void free_intset(struct intset *set) {
    if (!set) return;
    free_intset(set->left);
    free_intset(set->right);
    free(set);
}

void free_siterator(struct intset_iterator *it) {
    if (!it) return;
    free_array(it->stack);
    it->stack = NULL;
}
